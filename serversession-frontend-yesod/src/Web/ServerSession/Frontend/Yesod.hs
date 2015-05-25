module Web.ServerSession.Frontend.Yesod
  ( -- * Using server-side sessions
    simpleBackend
  , backend
    -- * Invalidating session IDs
  , forceInvalidate
  , ForceInvalidate(..)
  ) where

import Web.ServerSession.Core (ForceInvalidate(..))
import Web.ServerSession.Frontend.Yesod.Internal
