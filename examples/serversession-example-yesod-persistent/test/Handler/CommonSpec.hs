module Handler.CommonSpec (spec) where

import TestImport

spec :: Spec
spec = yesodSpecWithSiteGenerator mkApp $ do
    ydescribe "robots.txt" $ do
        yit "gives a 200" $ do
             get RobotsR
             statusIs 200
        yit "has correct User-agent" $ do
             get RobotsR
             bodyContains "User-agent: *"
    ydescribe "favicon.ico" $ do
        yit "gives a 200" $ do
             get FaviconR
             statusIs 200
