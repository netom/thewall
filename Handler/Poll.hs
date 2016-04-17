module Handler.Poll where

import Import
import State

getPollR :: Text -> Int -> Handler Value
getPollR key lastVersion = do
    yesod <- getYesod
    let ttl = appTtl $ appSettings yesod

    postList <- liftIO $ getNewPosts (appPosts yesod) key ttl lastVersion

    returnJson postList
