-- | @wai-session@ server-side session support.
--
-- Please note that this frontend has some limitations:
--
--   * Cookies use the @Max-age@ field instead of @Expires@.  The
--   @Max-age@ field is not supported by all browsers: some
--   browsers will treat them as non-persistent cookies.
--
--   * Also, the @Max-age@ is fixed and does not take a given a
--   session into consideration.
module Web.ServerSession.Frontend.Wai
  ( -- * Simple interface
    withServerSession
    -- * Invalidating session IDs
  , forceInvalidate
  , ForceInvalidate(..)
    -- * Flexible interface
  , sessionStore
  , createCookieTemplate
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
import Web.ServerSession.Frontend.Wai.Internal
