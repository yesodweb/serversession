-- | Core server-side session support.
module Web.ServerSession.Core
  ( -- * For serversession storage backends
    SessionId
  , Session(..)
  , Storage(..)

    -- * For serversession frontends
  , SessionMap
  , State(..)
  , createState
  , setAuthKey
  , loadSession
  , saveSession
  , SaveSessionToken
  , forceInvalidateKey
    -- ** To be re-exported by frontends
  , ForceInvalidate(..)
  ) where

import Web.ServerSession.Core.Internal
