module State where

import Import
import Data.HashMap
import Data.Time

addPost :: TVar PostMap -> Text -> Post -> IO ()
addPost tvpostmap key post = do
    now <- getCurrentTime
    let expires = addUTCTime (fromIntegral (86400 :: Int)) now -- TODO: config
    atomically $ do
        let updater Nothing = Just $ PostList expires [post]
            updater (Just (PostList _ ps)) = Just $ PostList expires (post : ps)
            in modifyTVar tvpostmap (alter updater key)

getPosts :: TVar PostMap -> Text -> IO PostList
getPosts tvpostmap key = do
    now <- getCurrentTime
    let expires = addUTCTime (fromIntegral (86400 :: Int)) now -- TODO: config
    atomically $ do
        postmap <- readTVar tvpostmap
        return $ findWithDefault (PostList expires []) key postmap
