-- | Yesod server-side session support.
module Web.ServerSession.Frontend.Yesod
  ( -- * Using server-side sessions
    simpleBackend
  , backend
    -- * Invalidating session IDs
  , forceInvalidate
  , ForceInvalidate(..)
    -- * State configuration
  , setCookieName
  , setAuthKey
  , setIdleTimeout
  , setAbsoluteTimeout
  , setTimeoutResolution
  , setPersistentCookies
  , setHttpOnlyCookies
  , setSecureCookies
  , State
  ) where

import Web.ServerSession.Core
import Web.ServerSession.Frontend.Yesod.Internal
