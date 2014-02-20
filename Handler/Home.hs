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
    defaultLayout $ do
        setTitle "Welcome To The Wall!"
        $(widgetFile "homepage")
