module Handler.Poll where

import Import
import State

import System.Timeout

getPollR :: Text -> Int -> Handler Value
getPollR key lastVersion = do
    yesod <- getYesod

    let ttl = appTtl $ appSettings yesod
    let to  = 25 * 1000000 :: Int

    mbPostList <- liftIO $ timeout to $ getNewPosts (appPosts yesod) key ttl lastVersion

    case mbPostList of
        Just postList -> returnJson postList
        Nothing       -> returnJson ([] :: [()])
