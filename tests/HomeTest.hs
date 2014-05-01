{-# LANGUAGE OverloadedStrings #-}
module HomeTest
    ( homeSpecs
    ) where

import TestImport

homeSpecs :: Spec
homeSpecs =
    ydescribe "Test the functionalities of the main page" $ do

        yit "loads the index and checks it looks right" $ do
            get HomeR
            statusIs 200
            htmlAllContain ".jumbotron h1" "Hey You"

            --request $ do
            --    setMethod "POST"
            --    setUrl HomeR
            --    addNonce
            --    fileByLabel "Choose a file" "tests/main.hs" "text/plain" -- talk about self-reference
            --    byLabel "What's on the file?" "Some Content"

            --statusIs 200
            --printBody
            --htmlCount ".message" 1
            --htmlAllContain ".message" "Some Content"
            --htmlAllContain ".message" "text/plain"
