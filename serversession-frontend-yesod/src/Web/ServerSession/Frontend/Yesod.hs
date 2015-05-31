-- | Yesod server-side session support.
--
-- This package implements an Yesod @SessionBackend@, so it's a
-- drop-in replacement for the default @clientsession@.
--
-- Unfortunately, Yesod currently provides no way of accessing
-- the session other than via its own functions.  If you want to
-- use a custom data type as your session data (instead of the
-- default @SessionMap@), it will have to implement
-- 'IsSessionMap' and you'll have to continue using Yesod's
-- session interface.
module Web.ServerSession.Frontend.Yesod
  ( -- * Using server-side sessions
    simpleBackend
  , backend
  , IsSessionMap(..)
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
