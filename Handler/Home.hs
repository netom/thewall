{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Welcome To The Wall!"
        $(widgetFile "homepage")
