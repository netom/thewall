module State where

import Import
import Data.HashMap

addPost :: TVar PostList -> Text -> Post -> IO ()
addPost tvpostmap key post = atomically $ do
    modifyTVar tvpostmap (alter updater key)
    where
        updater Nothing = Just [post]
        updater (Just ps) = Just (post : ps)

getPosts :: TVar PostList -> Text -> IO [Post]
getPosts tvpostmap key = atomically $ do
    postmap <- readTVar tvpostmap
    return $ findWithDefault [] key postmap
