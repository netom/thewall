module State where

import Import
import Data.HashMap
import Data.Time

addPost :: TVar PostMap -> Text -> Post -> Int -> IO ()
addPost tvpostmap key post ttl = do
    now <- getCurrentTime
    let expires = addUTCTime (fromIntegral ttl) now
    chan <- atomically $ do
        -- TODO: this is somewhat common in add and get, refactor.
        postmap <- readTVar tvpostmap
        let postlist = Data.HashMap.lookup key postmap

        case postlist of
            Nothing -> do
                chan <- newBroadcastTChan
                modifyTVar tvpostmap (alter (\_ -> Just $ PostList chan 0 expires [post]) key)
                return chan
            Just (PostList chan version _ ps) -> do
                modifyTVar tvpostmap (alter (\_ -> Just $ PostList chan (version + 1) expires (post : ps)) key)
                return chan

    atomically $ writeTChan chan EventNewPost

getPosts :: TVar PostMap -> Text -> Int -> IO PostList
getPosts tvpostmap key ttl = do
    now <- getCurrentTime

    let expires = addUTCTime (fromIntegral ttl) now

    pl <- atomically $ do
        postmap <- readTVar tvpostmap
        let postlist = Data.HashMap.lookup key postmap

        case postlist of
            Nothing -> do
                chan <- newBroadcastTChan
                let pl = PostList chan 0 expires []
                modifyTVar tvpostmap (alter (\_ -> Just pl) key)
                return pl
            Just pl -> return pl

    return pl

getNewPosts :: TVar PostMap -> Text -> Int -> Int -> IO PostList
getNewPosts tvpostmap key ttl lastVersion = do
    pl <- getPosts tvpostmap key ttl

    if lastVersion >= postListVersion pl
        then do
            mychan <- atomically $ dupTChan $ postListChannel pl
            -- Why I need to do these in separate transactions?
            _ <- atomically $ readTChan mychan
            getPosts tvpostmap key ttl
        else return pl

gcPosts :: TVar PostMap -> IO ()
gcPosts tvpostmap = do
    now <- getCurrentTime
    atomically $ modifyTVar tvpostmap (Data.HashMap.filter (\x -> postListExpire x >= now))
