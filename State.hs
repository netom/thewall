module State where

import Import
import Data.HashMap
import Data.Time

addPost :: TVar PostMap -> Text -> Post -> Int -> IO ()
addPost tvpostmap key post ttl = do
    now <- getCurrentTime
    let expires = addUTCTime (fromIntegral ttl) now -- TODO: config
    atomically $ do
        let updater Nothing = Just $ PostList expires [post]
            updater (Just (PostList _ ps)) = Just $ PostList expires (post : ps)
            in modifyTVar tvpostmap (alter updater key)

getPosts :: TVar PostMap -> Text -> Int -> IO PostList
getPosts tvpostmap key ttl = do
    now <- getCurrentTime
    let expires = addUTCTime (fromIntegral ttl) now -- TODO: config
    atomically $ do
        postmap <- readTVar tvpostmap
        return $ findWithDefault (PostList expires []) key postmap

gcPosts :: TVar PostMap -> IO ()
gcPosts tvpostmap = do
    now <- getCurrentTime
    atomically $ modifyTVar tvpostmap (Data.HashMap.filter (\x -> postListExpire x >= now))
