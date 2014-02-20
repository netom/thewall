{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import State

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Welcome To The Wall!"
        $(widgetFile "homepage")
