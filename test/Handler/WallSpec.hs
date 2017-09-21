{-# LANGUAGE OverloadedStrings #-}
module Handler.WallSpec (spec) where

import TestImport
import qualified Data.Text as T

spec :: Spec
spec = withApp $ do
    it "Loads an empty wall" $ do
        get $ WallR "this-is-a-test-wall"
        statusIs 200
        htmlAllContain ".wallfooter" "This wall will expire at"

    it "Posts to a wall" $ do
        get $ WallR "this-is-a-test-wall"
        request $ do
            setMethod "POST"
            setUrl $ WallR "this-is-a-test-wall"
            addPostParam "nick" "tester"
            addPostParam "body" "Some post content"

        statusIs 303 -- We should follow the PRG pattern
        assertHeader "Location" "/wall/this-is-a-test-wall"

        get $ WallR "this-is-a-test-wall"
        statusIs 200
        htmlCount "#posts .post" 1
        htmlAnyContain "#posts .post span" "tester"
        htmlAnyContain "#posts .post" "Some post content"

    it "Enforces nick length limit" $ do
        get $ WallR "this-is-a-test-wall"
        request $ do
            setMethod "POST"
            setUrl $ WallR "this-is-a-test-wall"
            addPostParam "nick" $ T.replicate 51 "a"
            addPostParam "body" "Some post content"

        statusIs 200
        htmlCount ".alert" 1
        htmlAnyContain ".alert" "Nick must be at most 50 characters long."

    it "Enforces post length limit" $ do
        get $ WallR "this-is-a-test-wall"
        request $ do
            setMethod "POST"
            setUrl $ WallR "this-is-a-test-wall"
            addPostParam "nick" "tester"
            addPostParam "body" $ T.replicate 201 "a"

        statusIs 200
        htmlCount ".alert" 1
        htmlAnyContain ".alert" "Post must be at most 200 characters long."

    it "Only allows 100 entries" $ do
        forM_ ([1..105]::[Int]) $ \i -> do
            get $ WallR "this-is-a-test-wall"
            request $ do
                setMethod "POST"
                setUrl $ WallR "this-is-a-test-wall"
                addPostParam "nick" "tester"
                addPostParam "body" $ T.pack $ show i
            statusIs 303

        get $ WallR "this-is-a-test-wall"

        htmlCount "#posts .post" 100
        htmlAnyContain "#posts .post" "105"
        htmlAnyContain "#posts .post" "6"
        -- TODO: there is no such thing as "does not contain"

    it "Gc will remove old entries" $ do
        -- TODO: can we mock time? This is ugly.

        -- Post one entry
        get $ WallR "this-is-a-test-wall"
        statusIs 200

        request $ do
            setMethod "POST"
            setUrl $ WallR "this-is-a-test-wall-1"
            addPostParam "nick" "tester"
            addPostParam "body" "Some post content 1"
        statusIs 303

        -- Wait 5 seconds and post an other
        liftIO $ threadDelay 5000000
        get $ WallR "this-is-a-test-wall"
        statusIs 200
        request $ do
            setMethod "POST"
            setUrl $ WallR "this-is-a-test-wall-2"
            addPostParam "nick" "tester"
            addPostParam "body" "Some post content 2"
        statusIs 303

        -- Wait 6 seconds, so the first wall will expire and be collected
        liftIO $ threadDelay 6000000

        -- First wall: should have been deleted
        get $ WallR "this-is-a-test-wall-1"
        statusIs 200
        htmlCount ".post" 0

        -- Second wall, should be still alive
        get $ WallR "this-is-a-test-wall-2"
        statusIs 200
        htmlCount ".post" 1

    it "Long poll works" $ do
        get $ WallR "this-is-a-test-wall"
        -- TODO
        -- We need to do a fork somehow here.
        -- Probably need a function to run a request in the background
