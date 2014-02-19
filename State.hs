module State where

import Import

addPost :: TVar PostList -> Text -> IO ()
addPost postlist post = atomically $ do
    modifyTVar postlist (\pl -> take 100 $ post : pl)

getPosts :: TVar PostList -> IO PostList
getPosts postlist = atomically $ do
    pl <- readTVar postlist
    return pl

incCounter :: TVar Integer -> IO Integer
incCounter tvcount = atomically $ do
    cnt <- readTVar tvcount
    modifyTVar tvcount (\c -> c + 1)
    return cnt
