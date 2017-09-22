{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    yesod <- getYesod

    let btcAddress = appBtcAddress $ appSettings yesod

    defaultLayout $ do
        setTitle "Welcome To The Wall!"
        $(widgetFile "homepage")
