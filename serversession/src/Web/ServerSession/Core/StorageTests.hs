{-# LANGUAGE Rank2Types #-}
-- | This module contains tests that should pass for every
-- storage backend.  These are not intended for end-users of the
-- @serversession@ library.  However, they are part of the
-- supported API, so they're not an @Internal@ module.
module Web.ServerSession.Core.StorageTests
  ( allStorageTests
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (Exception)
import Control.Monad
import Web.ServerSession.Core.Internal

import qualified Crypto.Nonce as N
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Time as TI


-- | Execute all storage tests using 'SessionMap'.
--
-- This function is meant to be used with @hspec@.  However, we
-- don't want to depend on @hspec@, so it takes the relevant
-- @hspec@ functions as arguments.  Here's how it should be
-- called:
--
-- @
-- allStorageTests myStorageBackend it runIO parallel shouldBe shouldReturn shouldThrow
-- @
--
-- Some storage backends are difficult to test with a clean
-- slate.  For this reason, this collection of tests works with
-- unclean storage backends.  In order to enforce these claims,
-- we always test with an unclean storage backend by getting a
-- single reference to it instead of asking for a function that
-- creates storage backends and calling it on every test.
--
-- In addition, this test suite can be executed in parallel,
-- there are no dependencies between tests.  However, some tests
-- require a large amount of memory so we try to run them
-- sequentially in order to reduce the peak memory usage of the
-- test suite.
allStorageTests
  :: forall m sto. (Monad m, Storage sto, SessionData sto ~ SessionMap)
  => sto                                                       -- ^ Storage backend.
  -> (String -> IO () -> m ())                                 -- ^ @hspec@'s it.
  -> (forall a. IO a -> m a)                                   -- ^ @hspec@'s runIO.
  -> (m () -> m ())                                            -- ^ @hspec@'s parallel
  -> (forall a. (Show a, Eq a) => a    -> a -> IO ())          -- ^ @hspec@'s shouldBe.
  -> (forall a. (Show a, Eq a) => IO a -> a -> IO ())          -- ^ @hspec@'s shouldReturn.
  -> (forall a e. Exception e => IO a -> (e -> Bool) -> IO ()) -- ^ @hspec@'s shouldThrow.
  -> m ()
allStorageTests storage it runIO parallel _shouldBe shouldReturn shouldThrow = do
  let run :: forall a. TransactionM sto a -> IO a
      run = runTransactionM storage

  gen <- runIO N.new

  parallel $ do
    -- runTransactionM
    it "runTransactionM should be sane" $ do
      run (return 42) `shouldReturn` (42 :: Int)

    -- getSession
    it "getSession should return Nothing for inexistent sessions" $ do
      replicateM_ 1000 $
        (generateSessionId gen >>= run . getSession storage)
          `shouldReturn` Nothing

    -- deleteSession
    it "deleteSession should not fail for inexistent sessions" $ do
      replicateM_ 1000 $
        generateSessionId gen >>= run . deleteSession storage

    it "deleteSession should delete the session" $ do
      replicateM_ 20 $ do
        s <- generateSession gen HasAuthId
        let sid = sessionKey s
        run (getSession storage sid) `shouldReturn` Nothing
        run (insertSession storage s)
        run (getSession storage sid) `shouldReturn` Just s
        run (deleteSession storage sid)
        run (getSession storage sid) `shouldReturn` Nothing


    -- deleteAllSessionsOfAuthId
    it "deleteAllSessionsOfAuthId should not fail for inexistent auth IDs" $ do
      replicateM_ 1000 $
        generateAuthId gen >>= run . deleteAllSessionsOfAuthId storage

    it "deleteAllSessionsOfAuthId should delete the relevant sessions (but no more)" $ do
      replicateM_ 20 $ do
        master <- generateSession gen HasAuthId
        let Just authId = sessionAuthId master
        preslaves <-
          (++) <$> replicateM 100 (generateSession gen HasAuthId)
               <*> replicateM 100 (generateSession gen NoAuthId)
        let slaves = (\s -> s { sessionAuthId = Just authId }) <$> preslaves
        others <-
          (++) <$> replicateM 30 (generateSession gen HasAuthId)
               <*> replicateM 30 (generateSession gen NoAuthId)
        let allS = master : slaves ++ others

        -- Insert preslaves then replace them with slaves to
        -- further test if the storage backend is able to maintain
        -- its invariants regarding auth IDs.
        run (mapM_ (insertSession storage) (master : preslaves ++ others))
        run (mapM_ (replaceSession storage) slaves)

        run (mapM (getSession storage . sessionKey) allS) `shouldReturn` (Just <$> allS)
        run (deleteAllSessionsOfAuthId storage authId)
        run (mapM (getSession storage . sessionKey) allS) `shouldReturn`
          ((Nothing <$ (master : slaves)) ++ (Just <$> others))

    -- insertSession
    it "getSession should return the contents of insertSession" $ do
      replicateM_ 20 $ do
        s <- generateSession gen HasAuthId
        run (getSession storage (sessionKey s)) `shouldReturn` Nothing
        run (insertSession storage s)
        run (getSession storage (sessionKey s)) `shouldReturn` Just s

    it "insertSession throws an exception if a session already exists" $ do
      replicateM_ 20 $ do
        s1 <- generateSession gen HasAuthId
        s2 <- generateSession gen HasAuthId
        let sid = sessionKey s1
            s3 = s2 { sessionKey = sid }
        run (getSession storage sid) `shouldReturn` Nothing
        run (insertSession storage s1)
        run (getSession storage sid) `shouldReturn` Just s1
        run (insertSession storage s3) `shouldThrow`
          (\(SessionAlreadyExists s1' s3' :: StorageException sto) ->
            s1 == s1' && s3 == s3')
        run (getSession storage sid) `shouldReturn` Just s1

    -- replaceSession
    it "getSession should return the contents of replaceSession" $ do
      replicateM_ 20 $ do
        s1  <- generateSession gen HasAuthId
        sxs <- replicateM 20 (generateSession gen HasAuthId)
        let sid = sessionKey s1
            sxs' = map (\s -> s { sessionKey = sid }) sxs
        run (getSession storage sid) `shouldReturn` Nothing
        run (insertSession storage s1)
        forM_ (zip (s1:sxs') sxs') $ \(before, after) -> do
          run (getSession storage sid) `shouldReturn` Just before
          run (replaceSession storage after)
          run (getSession storage sid) `shouldReturn` Just after

    it "replaceSession throws an exception if a session does not exist" $ do
      replicateM_ 20 $ do
        s <- generateSession gen HasAuthId
        let sid = sessionKey s
        run (getSession storage sid) `shouldReturn` Nothing
        run (replaceSession storage s) `shouldThrow`
          (\(SessionDoesNotExist s' :: StorageException sto) -> s == s')
        run (getSession storage sid) `shouldReturn` Nothing
        run (insertSession storage s)
        run (getSession storage sid) `shouldReturn` Just s
        let s2 = s { sessionAuthId = Nothing }
        run (replaceSession storage s2)
        run (getSession storage sid) `shouldReturn` Just s2
    -- End of call to 'parallel'

  -- Size and representation limits (not tested in parallel)
  let trySessionMap vals = do
        sid <- generateSessionId gen
        now <- TI.getCurrentTime
        let session = Session
              { sessionKey        = sid
              , sessionAuthId     = Nothing
              , sessionData       = SessionMap $ M.fromList vals
              , sessionCreatedAt  = now
              , sessionAccessedAt = now
              }
            ver2 = session { sessionData = SessionMap M.empty }
        run (getSession storage sid) `shouldReturn` Nothing
        run (insertSession storage session)
        run (getSession storage sid) `shouldReturn` (Just session)
        run (replaceSession storage ver2)
        run (getSession storage sid) `shouldReturn` (Just ver2)
        run (replaceSession storage session)
        run (getSession storage sid) `shouldReturn` (Just session)
        run (deleteSession storage sid)
        run (getSession storage sid) `shouldReturn` Nothing
      mib = 1024*1024

  it "stress test: one million small keys" $
    trySessionMap [(T.pack (show i), "bar") | i <- [1..(1000000 :: Int)]]

  it "stress test: one 100 MiB value" $
    trySessionMap [("foo", B.replicate (100 * mib) 70)]

  it "stress test: one 1 MiB key" $
    trySessionMap [(T.replicate mib "x", "foo")]

  it "stress test: key with all possible Unicode code points and value with all possible byte values" $
    trySessionMap [(T.pack [minBound..maxBound], B.pack [minBound..maxBound])]


-- | Generate a random auth ID for our tests.
generateAuthId :: N.Generator -> IO AuthId
generateAuthId = N.nonce128url


-- | Generate a random session for our tests.
generateSession :: N.Generator -> HasAuthId -> IO (Session SessionMap)
generateSession gen hasAuthId = do
  sid <- generateSessionId gen
  authId <-
    case hasAuthId of
      HasAuthId -> Just <$> generateAuthId gen
      NoAuthId  -> return Nothing
  data_ <- do
    keys <- replicateM 20 (N.nonce128urlT gen)
    vals <- replicateM 20 (N.nonce128url  gen)
    return $ M.fromList (zip keys vals)
  now <- TI.getCurrentTime
  return Session
    { sessionKey        = sid
    , sessionAuthId     = authId
    , sessionData       = SessionMap data_
    , sessionCreatedAt  = TI.addUTCTime (-1000) now
    , sessionAccessedAt = now
    }

data HasAuthId = HasAuthId | NoAuthId
