-- | Storage backend for @serversession@ using @acid-state@.
--
-- In order to use this backend, just open the 'AcidState'.  For
-- example:
--
-- @
-- import Control.Exception (bracket)
-- import Data.Acid.Local (openLocalState, createCheckpointAndClose)
-- import Web.ServerSession.Backend.Acid (AcidStorage(..), emptyState)
--
-- withSessionStorage :: (AcidStorage -> IO a) -> IO a
-- withSessionStorage =
--   bracket
--     ('AcidStorage' \<$\> openLocalState 'emptyState')
--     createCheckpointAndClose
-- @
module Web.ServerSession.Backend.Acid
  ( AcidStorage(..)
  , emptyState
  , ServerSessionAcidState
  ) where

import Web.ServerSession.Backend.Acid.Internal
