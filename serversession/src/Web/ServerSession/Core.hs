-- | Core server-side session support.
module Web.ServerSession.Core
  ( -- * For serversession storage backends
    SessionId
  , AuthId
  , Session(..)
  , Storage(..)
  , StorageException(..)

    -- * For serversession frontends
  , SessionMap
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
  , setPersistentCookies
  , setHttpOnlyCookies
  , setSecureCookies
  , ForceInvalidate(..)
  ) where

import Web.ServerSession.Core.Internal
