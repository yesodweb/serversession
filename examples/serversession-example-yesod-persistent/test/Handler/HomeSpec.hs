module Handler.HomeSpec (spec) where

import TestImport

-- The test was put on hold because the Example and the reality are too far apart.
-- If I find any bugs, I may add them here.
spec :: Spec
spec = return ()
-- spec = withApp $ do
--     it "loads the index and checks it looks right" $ do
--         get HomeR
--         statusIs 200

--         request $ do
--             setMethod "POST"
--             setUrl HomeR
--             addToken
--             fileByLabel "Choose a file" "test/Spec.hs" "text/plain" -- talk about self-reference
--             byLabel "What's on the file?" "Some Content"

--         statusIs 200
--         -- more debugging printBody
--         htmlCount ".message" 1
--         htmlAllContain ".message" "Some Content"
--         htmlAllContain ".message" "text/plain"

--     -- This is a simple example of using a database access in a test.  The
--     -- test will succeed for a fresh scaffolded site with an empty database,
--     -- but will fail on an existing database with a non-empty user table.
--     it "leaves the user table empty" $ do
--         get HomeR
--         statusIs 200
--         users <- runDB $ selectList ([] :: [Filter User]) []
--         assertEqual "user table empty" 0 $ length users
