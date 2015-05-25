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
import Web.Cookie (parseCookies, SetCookie(..))
import Web.PathPieces (toPathPiece)
import Web.ServerSession.Core
import Yesod.Core (MonadHandler)
import Yesod.Core.Handler (setSessionBS)
import Yesod.Core.Types (Header(AddCookie), SessionBackend(..))

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as TE
import qualified Network.Wai as W


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
--   makeSessionBackend = simpleBackend . SqlStorage . appConnPool
--   -- Do not forget to add migration code to your Application.hs!
--   -- Please check serversession-backend-persistent's documentation.
--   ...
-- @
simpleBackend
  :: (MonadIO m, Storage s)
  => s                        -- ^ Storage backend.
  -> m (Maybe SessionBackend) -- ^ Yesod session backend (always @Just@).
simpleBackend s = do
  state <- createState s
  let cookieName = "JSESSIONID" -- LOL :)
  return $ Just $ backend state cookieName


-- | Construct the server-side session backend using the given
-- state and cookie name.
backend
  :: Storage s
  => State s        -- ^ @serversession@ state, incl. storage backend.
  -> ByteString     -- ^ Cookie name.
  -> SessionBackend -- ^ Yesod session backend.
backend state cookieName =
  SessionBackend {
    sbLoadSession = \req -> do
      let rawSessionId = findSessionId cookieName req
      (sessionMap, saveSessionToken) <- loadSession state rawSessionId
      let save =
            fmap ((:[]) . createCookie cookieName) .
            saveSession state saveSessionToken
      return (sessionMap, save)
  }


-- | Create a cookie for the given session ID.
createCookie :: ByteString -> SessionId -> Header
createCookie cookieName key =
  -- Generate a cookie with the final session ID.
  AddCookie def
    { setCookieName     = cookieName
    , setCookieValue    = TE.encodeUtf8 $ toPathPiece key
    , setCookiePath     = Just "/"
    , setCookieExpires  = Just undefined
    , setCookieDomain   = Nothing
    , setCookieHttpOnly = True
    }


-- | Fetch the 'SessionId' from the cookie with the given name.
-- Returns @Nothing@ if:
--
--   * There are zero cookies with the given name.
--
--   * There is more than one cookie with the given name.
findSessionId :: ByteString -> W.Request -> Maybe ByteString
findSessionId cookieName req = do
  [raw] <- return $ do
    ("Cookie", header) <- W.requestHeaders req
    (k, v) <- parseCookies header
    guard (k == cookieName)
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
