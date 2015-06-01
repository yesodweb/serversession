module Main (main) where

import Control.Applicative ((<$>))
import Data.Acid.Local (openLocalState, createCheckpointAndClose)
import Data.Acid.Memory (openMemoryState)
import Test.Hspec
import Web.ServerSession.Backend.Acid
import Web.ServerSession.Core.StorageTests

import qualified Control.Exception as E

main :: IO ()
main =
  E.bracket
    (AcidStorage <$> openLocalState emptyState)
    (createCheckpointAndClose . acidState) $
    \acidLocal -> hspec $ do
      acidMem <- runIO $ AcidStorage <$> openMemoryState emptyState
      describe "AcidStorage on memory only" $
        allStorageTests acidMem it runIO parallel shouldBe shouldReturn shouldThrow
      describe "AcidStorage on local storage" $
#if MIN_VERSION_base(4,8,0)
        allStorageTests acidLocal it runIO parallel shouldBe shouldReturn shouldThrow
#else
        it "is not tested on GHC < 7.10.1" $ do
          pendingWith "<https://github.com/acid-state/acid-state/issues/55>"
#endif
