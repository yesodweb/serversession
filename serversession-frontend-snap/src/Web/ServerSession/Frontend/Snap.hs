-- | Snap server-side session support.
module Web.ServerSession.Frontend.Snap
  ( -- * Using server-side sessions
    initServerSessionManager
  , simpleServerSessionManager
    -- * Invalidating session IDs
  , forceInvalidate
  , ForceInvalidate(..)
    -- * State configuration
  , setCookieName
  , setAuthKey
  , setIdleTimeout
  , setAbsoluteTimeout
  , setPersistentCookies
  , setHttpOnlyCookies
  , setSecureCookies
  , State
  ) where

import Web.ServerSession.Core
import Web.ServerSession.Frontend.Snap.Internal
