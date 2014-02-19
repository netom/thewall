module State where

import Import

type Postlist = [Text]

incCounter :: TVar Integer -> IO Integer
incCounter tvcount = atomically $ do
    cnt <- readTVar tvcount
    modifyTVar' tvcount (\c -> c + 1)
    return cnt
