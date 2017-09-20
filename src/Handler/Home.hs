{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import State

getHomeR :: Handler Html
getHomeR = do
    yesod <- getYesod

    let btcAddress = appBtcAddress $ appSettings yesod

    randomWall <- liftIO getRandomWall
    renderfunc <- getUrlRender

    let randomWallLink = renderfunc $ WallR randomWall

    qrpng <- liftIO $ getQRPng $ unpack randomWallLink

    defaultLayout $ do
        setTitle "Welcome To The Wall!"
        $(widgetFile "homepage")
