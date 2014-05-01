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
