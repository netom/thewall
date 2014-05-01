{-# LANGUAGE OverloadedStrings #-}
module WallTest
    ( wallSpecs
    ) where

import TestImport
import qualified Data.Text as T
import Control.Monad

-- TODO: don't hard-wire constants.
-- TODO: how to check translated texts?

wallSpecs :: Spec
wallSpecs =
    ydescribe "Test the functionality of a wall" $ do

        yit "Loads an empty wall" $ do
            get $ WallR "this-is-a-test-wall"
            statusIs 200
            htmlAllContain ".container .well p" "This wall will expire at"

        yit "Posts to a wall" $ do
            get $ WallR "this-is-a-test-wall"
            request $ do
                setMethod "POST"
                setUrl $ WallR "this-is-a-test-wall"
                addNonce
                byLabel "Nick" "tester"
                byLabel "Post" "Some post content"

            statusIs 302 -- We should follow the PRG pattern
            assertHeader "Location" "http://localhost:3000/wall/this-is-a-test-wall" --TODO: use value from settings

            get $ WallR "this-is-a-test-wall"
            statusIs 200
            htmlCount "#posts .post" 1
            htmlAnyContain "#posts .post span" "tester"
            htmlAnyContain "#posts .post" "Some post content"

        yit "Enforces nick length limit" $ do
            get $ WallR "this-is-a-test-wall"
            request $ do
                setMethod "POST"
                setUrl $ WallR "this-is-a-test-wall"
                addNonce
                byLabel "Nick" $ T.replicate 51 "a"
                byLabel "Post" "Some post content"

            statusIs 200
            htmlCount ".errors" 1
            htmlAnyContain ".errors" "Nick must be at most 50 characters long."

        yit "Enforces post length limit" $ do
            get $ WallR "this-is-a-test-wall"
            request $ do
                setMethod "POST"
                setUrl $ WallR "this-is-a-test-wall"
                addNonce
                byLabel "Nick" "tester"
                byLabel "Post" $ T.replicate 201 "a"

            statusIs 200
            htmlCount ".errors" 1
            htmlAnyContain ".errors" "Post must be at most 200 characters long."

        yit "Only allows 100 entries" $ do
            forM_ [1..105] $ \i -> do
                get $ WallR "this-is-a-test-wall"
                request $ do
                    setMethod "POST"
                    setUrl $ WallR "this-is-a-test-wall"
                    addNonce
                    byLabel "Nick" "tester"
                    byLabel "Post" $ T.pack $ show i
                statusIs 302

            get $ WallR "this-is-a-test-wall"

            htmlCount "#posts .post" 100
            htmlAnyContain "#posts .post" "105"
            htmlAnyContain "#posts .post" "6"
            --TODO: there is no such thing as "does not contain"
