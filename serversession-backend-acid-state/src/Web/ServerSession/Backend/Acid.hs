-- | Storage backend for @serversession@ using @acid-state@.
--
-- In order to use this backend, just open the 'AcidState'.  For
-- example:
--
-- @
-- import Data.Acid
-- import Web.ServerSession.Backend.Acid
--
-- makeSessionStorage :: IO 'AcidStorage'
-- makeSessionStorage =
--   'AcidStorage' \<$\> openLocalState 'emptyState'
-- @
module Web.ServerSession.Backend.Acid
  ( AcidStorage(..)
  , emptyState
  , ServerSessionAcidState
  ) where

import Web.ServerSession.Backend.Acid.Internal
