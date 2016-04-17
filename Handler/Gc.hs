module Handler.Gc where

import Import
import State

getGcR :: Text -> Handler Html
getGcR key = do
    yesod <- getYesod
    let syskey = appSyskey $ appSettings yesod
    if syskey == key
        then do
            liftIO $ gcPosts (appPosts yesod)
            return "OK"
        else do
            return "ERR"
