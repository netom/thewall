module State where

import Import
import Data.HashMap
import Data.Time

addPost :: TVar PostMap -> Text -> Post -> Int -> IO ()
addPost tvpostmap key post ttl = do
    now <- getCurrentTime
    let expires = addUTCTime (fromIntegral ttl) now
    chan <- atomically $ do
        postmap <- readTVar tvpostmap
        let postlist = Data.HashMap.lookup key postmap

        case postlist of
            Nothing -> do
                chan <- newBroadcastTChan
                modifyTVar tvpostmap (alter (\_ -> Just $ PostList chan expires [post]) key)
                return chan
            Just (PostList chan _ ps) -> do
                modifyTVar tvpostmap (alter (\_ -> Just $ PostList chan expires (post : ps)) key)
                return chan

    atomically $ writeTChan chan EventNewPost

getPosts :: Bool -> TVar PostMap -> Text -> Int -> IO PostList
getPosts wait tvpostmap key ttl = do
    now <- getCurrentTime

    let expires = addUTCTime (fromIntegral ttl) now

    pl <- atomically $ do
        postmap <- readTVar tvpostmap

        let postlist = Data.HashMap.lookup key postmap

        case postlist of
            Nothing -> do
                chan <- newBroadcastTChan
                let pl = PostList chan expires []
                modifyTVar tvpostmap (alter (\_ -> Just pl) key)
                return pl
            Just pl -> return pl

    plw <- if wait
        then do
            mychan <- atomically $ dupTChan $ postListChannel pl
            -- Why I need to do these in separate transactions?
            _ <- atomically $ readTChan mychan
            -- TODO: REFACTOOOOOOOOOOOOOORRRRRRRRRRRRRRRRRRRRRRR
            postmap <- atomically $ readTVar tvpostmap

            let postlist = Data.HashMap.lookup key postmap

            case postlist of
                Nothing -> atomically $ do
                    chan <- newBroadcastTChan
                    let pl = PostList chan expires []
                    modifyTVar tvpostmap (alter (\_ -> Just pl) key)
                    return pl
                Just pl -> return pl

        else return pl

    return plw

gcPosts :: TVar PostMap -> IO ()
gcPosts tvpostmap = do
    now <- getCurrentTime
    atomically $ modifyTVar tvpostmap (Data.HashMap.filter (\x -> postListExpire x >= now))
