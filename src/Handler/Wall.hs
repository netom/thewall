{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Wall where

import Import
import State
import Yesod.WebSockets
import Control.Concurrent
import Data.Aeson

import qualified Network.WebSockets as WS

getWallR :: Text -> Handler Html
getWallR key = do
    yesod <- getYesod

    let tvpostmap   = appPosts yesod
    let settings    = appSettings yesod

    let ttl         = appTtl settings
    let postMaxSize = appPostMaxSize settings

    webSockets $ do
        conn <- ask

        -- 1. Fork a thread that waits on the TChan of the wall key and send posts to the browser
        _ <- liftIO $ forkIO $ do
            pl <- getPosts tvpostmap key ttl        
            mychan <- atomically $ dupTChan $ postListChannel pl

            let txLoop = do
                 msg <- atomically $ readTChan mychan
                 WS.sendTextData conn $ encode msg
                 txLoop
                 in txLoop

        -- 2. Receive data from the connection: 
        let rxLoop = do
             mbMsg <- liftIO $ try $ WS.receiveData conn :: (WS.WebSocketsData a) => IO (Either SomeException a)
             case mbMsg of
                 Right msg -> do
                    case decode msg of
                        Just (WsList wsKey) -> do
                            pl <- getPosts tvpostmap wsKey ttl
                            forM_ (reverse $ postListPosts pl) $ WS.sendTextData conn . encode . WsPost wsKey
                        Just (WsPost wsKey wsPost) -> do
                            addPost tvpostmap wsKey wsPost ttl
                        Just (WsDelete wsKey) -> do
                            deletePosts tvpostmap wsKey ttl
                            WS.sendTextData conn msg
                        _ -> return () -- The message couldn't be decoded, just swallow the error and continue.
                    rxLoop -- We're good, continue serving
                 Left _ -> return () -- An exception were thrown, leave the connection.
             in liftIO $ rxLoop

    urlRender <- getUrlRender

    defaultLayout $ do
        $(widgetFile "wall")
