module Handler.Poll where

import Import
import State

getPollR :: Text -> Int -> Handler Value
getPollR key lastVersion = do
    yesod <- getYesod
    ttl <- fmap extraTtl getExtra

    postList <- liftIO $ getNewPosts (posts yesod) key ttl lastVersion

    returnJson postList
