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

import qualified Database.Persist.TH as P
import qualified Database.Persist.Sql as P

P.mkMigrate "migrateAll" (serverSessionDefs (Proxy :: Proxy SessionMap))

main :: IO ()
main = hspec $
  forM_ [ ("PostgreSQL", createPostgresqlPool "user=test dbname=test password=test" 20)
        , ("SQLite",     createSqlitePool "test.db" 1) ] $
    \(rdbms, createPool) ->
  describe ("SqlStorage on " ++ rdbms) $ do
    pool <-
      runIO $ do
        pool <- runNoLoggingT createPool
        runStderrLoggingT $ P.runSqlPool (P.runMigration migrateAll) pool
        return pool
    afterAll_ (destroyAllResources pool) $
      allStorageTests (SqlStorage pool) it runIO parallel shouldBe shouldReturn shouldThrow
