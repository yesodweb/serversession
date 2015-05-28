module Main (main) where

import Database.Redis (connect, defaultConnectInfo)
import Test.Hspec
import Web.ServerSession.Backend.Redis
import Web.ServerSession.Core.StorageTests

import qualified Control.Exception as E

main :: IO ()
main = do
  conn <- connect defaultConnectInfo
  hspec $ describe "RedisStorage" $ parallel $
    allStorageTests (RedisStorage conn) it runIO shouldBe shouldReturn shouldThrow
