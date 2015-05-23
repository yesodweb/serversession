-- | Server-side session backend.
--
-- This module is meant to be imported qualified:
--
-- @
-- import qualified Yesod.Persist.Session as Session
-- @
--
-- TODO: Usage
module Yesod.Persist.Session
  ( backend
  , createState
  , State
  , forceInvalidate
  , ForceInvalidate(..)
  ) where

import Yesod.Persist.Session.Internal.Backend
