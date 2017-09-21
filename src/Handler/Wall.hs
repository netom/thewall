{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts  #-}
module Handler.Wall where

import Import
import State
import Data.Text
import Data.Time
import Yesod.WebSockets

import qualified Network.WebSockets as WS
import qualified Data.Text as T

-- Build a FieldSettings according to our common pattern:
-- id and name are the same, label is empty, and there is a placeholder text
fsPIN :: Text -> Text -> FieldSettings master
fsPIN name ph = FieldSettings "" Nothing (Just name) (Just name) [("placeholder", ph)]

wallForm :: Html -> FieldView App -> FieldView App -> Widget
wallForm extra nickView bodyView = do
    $(widgetFile "wallForm")

-- This form is used to post / process a message
postForm :: Maybe Post -> Html -> MForm Handler (FormResult Post, Widget)
postForm post extra = do
    (nickRes, nickView) <- mreq nickField (fsPIN "nick" "choose a nickname") (postNick <$> post)
    (bodyRes, bodyView) <- mreq postField (fsPIN "body" "type your message") (postBody <$> post)

    now <- liftIO getCurrentTime

    let postRes = Post now <$> nickRes <*> bodyRes

    return (postRes, wallForm extra nickView bodyView)
  where
    nickField = check validateNick textField
    postField = check validatePost textareaField

    nickLengthError = "Nick must be at most 50 characters long." :: Text
    postLengthError = "Post must be at most 200 characters long." :: Text

    validateNick n
        | Data.Text.length n > 50 = Left nickLengthError
        | otherwise               = Right n

    validatePost p
        | Data.Text.length (unTextarea p) > 200 = Left postLengthError
        | otherwise               = Right p

getWallR :: Text -> Handler Html
getWallR key = do
    yesod <- getYesod

    let ttl = appTtl $ appSettings yesod
    let tvpostmap = appPosts yesod

    webSockets $ do
        conn <- ask

        -- 1. Fork a thread that waits on the TChan of the wall key and send posts to the browser
        liftIO $ forkIO $ do
            pl <- getPosts tvpostmap key ttl        
            mychan <- atomically $ dupTChan $ postListChannel pl

            let txLoop = do
                msg <- atomically $ readTChan mychan
                WS.sendTextData conn $ encode msg
                -- Will this terminate, or throw an error?
            in txLoop

        -- 2. Receive data from the connection: 
        let rxLoop = do
             mbMsg <- liftIO $ try $ WS.receiveData conn :: (WS.WebSocketsData a) => IO (Either SomeException a)
             case mbMsg of
                 Right msg -> do
                    case decode msg of
                        Just (WsList key) -> do
                            putStrLn $ "Received list command, sending list of posts"
                            -- TODO
                        Just (WsPost key post) -> do
                            putStrLn $ "Received post command, adding post"
                            -- TODO
                        _ -> do
                            -- The message couldn't be decoded, just swallow it and continue.
                            putStrLn $ "Error parsing message"
                    rxLoop -- We're good, continue serving
                 Left _ -> do
                    -- An exception were thrown, leave the connection.
                    putStrLn $ "Exception encountered, closing connection"
             in liftIO $ rxLoop -- TODO: does forkIO really work here?

        liftIO $ putStrLn "Done with this."

    renderfunc <- getUrlRender
    aurl <- getCurrentRoute

    let surl = case aurl of
                   Just u -> renderfunc u
                   Nothing -> ""

    qrpng <- liftIO $ getQRPng $ T.unpack surl
    -- ---------------------------------------------

    maybeNick <- lookupSession (T.concat ["nick", surl])

    let formNick = case maybeNick of
                   Nothing -> ""
                   Just nick -> nick

    let post = Post (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)) formNick (Textarea "")

    (widget, enctype) <- generateFormPost $ postForm $ Just post

    yesod <- getYesod
    let ttl = appTtl $ appSettings yesod

    PostList _ version expire ps <- liftIO $ getPosts (appPosts yesod) key ttl

    defaultLayout $ do
        $(widgetFile "wall")

postWallR :: Text -> Handler Html
postWallR key = do
    -- TODO: refactor this into a separate function
    renderfunc <- getUrlRender
    aurl <- getCurrentRoute

    let surl = case aurl of
                   Just u -> renderfunc u
                   Nothing -> ""

    qrpng <- liftIO $ getQRPng $ T.unpack surl
    -- ---------------------------------------------

    ((result, widget), enctype) <- runFormPost $ postForm Nothing

    yesod <- getYesod
    let ttl = appTtl $ appSettings yesod

    case result of
        FormSuccess post -> do
            let Post _ nick _ = post
            setSession (T.concat ["nick", surl]) nick
            liftIO $ addPost (appPosts yesod) key post ttl

            redirect (WallR key)
        FormFailure _ -> do
            PostList _ version expire ps <- liftIO $ getPosts (appPosts yesod) key ttl
            defaultLayout $ do
                $(widgetFile "wall")
        FormMissing -> do
            setMessage "No form data"
            redirect (WallR key)
