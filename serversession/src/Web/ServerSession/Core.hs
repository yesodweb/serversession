-- | Core server-side session support.
module Web.ServerSession.Core
  ( -- * For serversession storage backends
    SessionId
  , Session(..)
  , Storage(..)

    -- * For serversession frontends
  , SessionMap
  , State
  , createState
  , loadSession
  , saveSession
  , SaveSessionToken
  , forceInvalidateKey
    -- ** To be re-exported by frontends
  , setCookieName
  , setAuthKey
  , setIdleTimeout
  , setAbsoluteTimeout
  , ForceInvalidate(..)
  ) where

import Web.ServerSession.Core.Internal
