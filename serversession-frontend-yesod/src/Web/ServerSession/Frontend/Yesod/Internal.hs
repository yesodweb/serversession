-- | Internal module exposing the guts of the package.  Use at
-- your own risk.  No API stability guarantees apply.
module Web.ServerSession.Frontend.Yesod.Internal
  ( simpleBackend
  , backend
  , createCookie
  , findSessionId
  , forceInvalidate
  ) where

import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Default (def)
import Web.PathPieces (toPathPiece)
import Web.ServerSession.Core
import Web.ServerSession.Core.Internal (cookieName, httpOnlyCookies, secureCookies)
import Yesod.Core (MonadHandler)
import Yesod.Core.Handler (setSessionBS)
import Yesod.Core.Types (Header(AddCookie), SessionBackend(..))

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as TE
import qualified Network.Wai as W
import qualified Web.Cookie as C


-- | Construct the server-side session backend using
-- the given storage backend.
--
-- Example usage for the Yesod scaffold using
-- @serversession-backend-persistent@:
--
-- @
-- import Web.ServerSession.Backend.Persistent (SqlStorage(..))
-- import Web.ServerSession.Frontend.Yesod (simpleBackend)
--
-- instance Yesod App where
--   ...
--   makeSessionBackend = simpleBackend id . SqlStorage . appConnPool
--   -- Do not forget to add migration code to your Application.hs!
--   -- Please check serversession-backend-persistent's documentation.
--   ...
-- @
--
-- For example, if you wanted to disable the idle timeout,
-- decrease the absolute timeout to one day and mark cookies as
-- \"Secure\", you could change that line to:
--
-- @
--   makeSessionBackend = simpleBackend opts . SqlStorage . appConnPool
--     where opts = setIdleTimeout Nothing
--                . setAbsoluteTimeout (Just $ 60*60*24)
--                . setSecureCookies True
-- @
simpleBackend
  :: (MonadIO m, Storage s)
  => (State s -> State s)     -- ^ Set any options on the @serversession@ state.
  -> s                        -- ^ Storage backend.
  -> m (Maybe SessionBackend) -- ^ Yesod session backend (always @Just@).
simpleBackend opts s =
  return . Just . backend . opts =<< createState s


-- | Construct the server-side session backend using the given
-- state.
backend
  :: Storage s
  => State s        -- ^ @serversession@ state, incl. storage backend.
  -> SessionBackend -- ^ Yesod session backend.
backend state =
  SessionBackend {
    sbLoadSession = \req -> do
      let rawSessionId = findSessionId cookieNameBS req
      (sessionMap, saveSessionToken) <- loadSession state rawSessionId
      let save =
            fmap ((:[]) . createCookie state cookieNameBS) .
            saveSession state saveSessionToken
      return (sessionMap, save)
  }
  where
    cookieNameBS = TE.encodeUtf8 $ cookieName state


-- | Create a cookie for the given session ID.
--
-- The cookie expiration is set via 'nextExpires'.  Note that this is just an optimization
createCookie :: State s -> ByteString -> Session -> Header
createCookie state cookieNameBS session =
  -- Generate a cookie with the final session ID.
  AddCookie def
    { C.setCookieName     = cookieNameBS
    , C.setCookieValue    = TE.encodeUtf8 $ toPathPiece $ sessionKey session
    , C.setCookiePath     = Just "/"
    , C.setCookieExpires  = cookieExpires state session
    , C.setCookieDomain   = Nothing
    , C.setCookieHttpOnly = httpOnlyCookies state
    , C.setCookieSecure   = secureCookies state
    }


-- | Fetch the 'SessionId' from the cookie with the given name.
-- Returns @Nothing@ if:
--
--   * There are zero cookies with the given name.
--
--   * There is more than one cookie with the given name.
findSessionId :: ByteString -> W.Request -> Maybe ByteString
findSessionId cookieNameBS req = do
  [raw] <- return $ do
    ("Cookie", header) <- W.requestHeaders req
    (k, v) <- C.parseCookies header
    guard (k == cookieNameBS)
    return v
  return raw


-- | Invalidate the current session ID (and possibly more, check
-- 'ForceInvalidate').  This is useful to avoid session fixation
-- attacks (cf. <http://www.acrossecurity.com/papers/session_fixation.pdf>).
--
-- Note that the invalidate /does not/ occur when the call to
-- this action is made!  The sessions will be invalidated on the
-- end of the handler processing.  This means that later calls to
-- 'forceInvalidate' on the same handler will override earlier
-- calls.
--
-- This function works by setting a session variable that is
-- checked when saving the session.  The session variable set by
-- this function is then discarded and is not persisted across
-- requests.
forceInvalidate :: MonadHandler m => ForceInvalidate -> m ()
forceInvalidate = setSessionBS forceInvalidateKey . B8.pack . show
