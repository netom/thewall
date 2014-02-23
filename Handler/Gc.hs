module Handler.Gc where

import Import
import State

getGcR :: Text -> Handler Html
getGcR key = do
    syskey <- fmap extraSyskey getExtra
    yesod <- getYesod
    
    if syskey == key
        then do
            liftIO $ gcPosts (posts yesod)
            return "OK"
        else do
            return "ERR"
    
