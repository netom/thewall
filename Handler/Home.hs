{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import State

getHomeR :: Handler Html
getHomeR = do
    randomWall <- liftIO getRandomWall
    renderfunc <- getUrlRender

    let randomWallLink = renderfunc $ WallR randomWall

    qrpng <- liftIO $ getQRPng $ unpack randomWallLink

    defaultLayout $ do
        setTitle "Welcome To The Wall!"
        $(widgetFile "homepage")
