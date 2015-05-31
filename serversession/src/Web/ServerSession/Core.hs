-- | Core server-side session support.
module Web.ServerSession.Core
  ( -- * For serversession storage backends
    SessionId
  , AuthId
  , Session(..)
  , Storage(..)
  , StorageException(..)
  , IsSessionData(..)
  , DecomposedSession(..)

    -- * For serversession frontends
  , SessionMap(..)
  , State
  , createState
  , getCookieName
  , getHttpOnlyCookies
  , getSecureCookies
  , loadSession
  , cookieExpires
  , saveSession
  , SaveSessionToken
  , forceInvalidateKey
    -- ** To be re-exported by frontends
  , setCookieName
  , setAuthKey
  , setIdleTimeout
  , setAbsoluteTimeout
  , setTimeoutResolution
  , setPersistentCookies
  , setHttpOnlyCookies
  , setSecureCookies
  , ForceInvalidate(..)
  ) where

import Web.ServerSession.Core.Internal
