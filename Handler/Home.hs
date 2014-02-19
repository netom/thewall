{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import State

data Post = Post Text

postForm :: Html -> MForm Handler (FormResult Post, Widget)
postForm = renderDivs $ Post
    <$> areq textField "post" Nothing

getHomeR :: Handler Html
getHomeR = do
    (widget, enctype) <- generateFormPost postForm

    yesod <- getYesod

    pl <- liftIO $ getPosts (posts yesod) "123123 key 123123"

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, _), _) <- runFormPost postForm

    yesod <- getYesod

    let FormSuccess (Post post) = result

    liftIO $ addPost (posts yesod) "123123 key 123123" post

    redirect HomeR
