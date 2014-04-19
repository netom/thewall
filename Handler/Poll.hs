module Handler.Poll where

import Import
import State
import Data.Text

getPollR :: Text -> Handler Value
getPollR key = do
    yesod <- getYesod
    ttl <- fmap extraTtl getExtra

    postList <- liftIO $ getPosts True (posts yesod) key ttl

    returnJson postList
