module Main (main) where

import Database.Redis (connect, defaultConnectInfo)
import Test.Hspec
import Web.ServerSession.Backend.Redis
import Web.ServerSession.Core.StorageTests

main :: IO ()
main = do
  conn <- connect defaultConnectInfo
  hspec $ describe "RedisStorage" $
    allStorageTests (RedisStorage conn) it runIO parallel shouldBe shouldReturn shouldThrow
