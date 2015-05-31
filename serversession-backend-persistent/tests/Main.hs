module Main (main) where

import Control.Monad (forM_)
import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT)
import Data.Pool (destroyAllResources)
import Data.Proxy (Proxy(..))
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sqlite (createSqlitePool)
import Test.Hspec
import Web.ServerSession.Backend.Persistent
import Web.ServerSession.Core (SessionMap)
import Web.ServerSession.Core.StorageTests

import qualified Control.Exception as E
import qualified Database.Persist.TH as P
import qualified Database.Persist.Sql as P

P.mkMigrate "migrateAll" (serverSessionDefs (Proxy :: Proxy SessionMap))

main :: IO ()
main = hspec $
  forM_ [ ("PostgreSQL", createPostgresqlPool "user=test dbname=test password=test" 20)
        , ("SQLite",     createSqlitePool "test.db" 1) ] $
    \(rdbms, createPool) ->
  describe ("SqlStorage on " ++ rdbms) $ do
    epool <-
      runIO $ E.try $ do
        pool <- runNoLoggingT createPool
        runStderrLoggingT $ P.runSqlPool (P.runMigration migrateAll) pool
        return pool
    case epool of
      Left (E.SomeException exc) ->
        it "failed to create connection or migrate database" $
          pendingWith (show exc)
      Right pool ->
        afterAll_ (destroyAllResources pool) $
          allStorageTests (SqlStorage pool) it runIO parallel shouldBe shouldReturn shouldThrow
