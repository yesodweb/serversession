-- | Internal module exposing the guts of the package.  Use at
-- your own risk.  No API stability guarantees apply.
module Web.ServerSession.Frontend.Wai.Internal
  ( withServerSession
  , sessionStore
  , mkSession
  , KeyValue(..)
  , createCookieTemplate
  , calculateMaxAge
  , forceInvalidate
  ) where

import Control.Applicative ((<$>))
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString)
import Data.Default (def)
import Data.Text (Text)
import Web.PathPieces (toPathPiece)
import Web.ServerSession.Core
import Web.ServerSession.Core.Internal (absoluteTimeout, idleTimeout, persistentCookies)

import qualified Data.ByteString.Char8 as B8
import qualified Data.IORef as I
import qualified Data.Map as M
import qualified Data.Text.Encoding as TE
import qualified Data.Time as TI
import qualified Data.Vault.Lazy as V
import qualified Network.Wai as W
import qualified Network.Wai.Session as WS
import qualified Web.Cookie as C


-- | Construct the @wai-session@ middleware using the given
-- storage backend and options.  This is a convenient function
-- that uses 'WS.withSession', 'createState', 'sessionStore',
-- 'getCookieName' and 'createCookieTemplate'.
withServerSession
  :: (MonadIO m, MonadIO n, Storage sto, SessionData sto ~ SessionMap)
  => V.Key (WS.Session m Text ByteString) -- ^ 'V.Vault' key to use when passing the session through.
  -> (State sto -> State sto)             -- ^ Set any options on the @serversession@ state.
  -> sto                                  -- ^ Storage backend.
  -> n W.Middleware
withServerSession key opts storage = liftIO $ do
  st <- opts <$> createState storage
  return $
    WS.withSession
      (sessionStore st)
      (TE.encodeUtf8 $ getCookieName st)
      (createCookieTemplate st)
      key


-- | Construct the @wai-session@ session store using the given
-- state.  Note that keys and values types are fixed.
--
-- As @wai-session@ always requires a value to be provided, we
-- return an empty @ByteString@ when the empty session was not
-- saved.
sessionStore
  :: (MonadIO m, Storage sto, KeyValue (SessionData sto))
  => State sto -- ^ @serversession@ state, incl. storage backend.
  -> WS.SessionStore m (Key (SessionData sto)) (Value (SessionData sto))
     -- ^ @wai-session@ session store.
sessionStore state =
  \mcookieVal -> do
    (data1, saveSessionToken) <- loadSession state mcookieVal
    sessionRef <- I.newIORef data1
    let save = do
          data2 <- I.atomicModifyIORef' sessionRef $ \a -> (a, a)
          msession <- saveSession state saveSessionToken data2
          return $ maybe "" (TE.encodeUtf8 . toPathPiece . sessionKey) msession
    return (mkSession sessionRef, save)


-- | Build a 'WS.Session' from an 'I.IORef' containing the
-- session data.
mkSession :: (MonadIO m, KeyValue sess) => I.IORef sess -> WS.Session m (Key sess) (Value sess)
mkSession sessionRef =
  -- We need to use atomicModifyIORef instead of readIORef
  -- because latter may be reordered (cf. "Memory Model" on
  -- Data.IORef's documentation).
  ( \k   -> kvLookup k <$> liftIO (I.atomicModifyIORef' sessionRef $ \a -> (a, a))
  , \k v -> liftIO (I.atomicModifyIORef' sessionRef $ flip (,) () . kvInsert k v)
  )


----------------------------------------------------------------------


-- | Class for session data types that can be used as key-value
-- stores.
class IsSessionData sess => KeyValue sess where
  type Key   sess :: *
  type Value sess :: *
  kvLookup :: Key sess -> sess -> Maybe (Value sess)
  kvInsert :: Key sess -> Value sess -> sess -> sess


instance KeyValue SessionMap where
  type Key   SessionMap = Text
  type Value SessionMap = ByteString
  kvLookup k = M.lookup k . unSessionMap
  kvInsert k v (SessionMap m) = SessionMap (M.insert k v m)


----------------------------------------------------------------------


-- | Create a cookie template given a state.
--
-- Since we don't have access to the 'Session', we can't fill the
-- @Expires@ field.  Besides, as the template is constant,
-- eventually the @Expires@ field would become outdated.  This is
-- a limitation of @wai-session@'s interface, not a
-- @serversession@ limitation.  Other frontends support the
-- @Expires@ field.
--
-- Instead, we fill only the @Max-age@ field.  It works fine for
-- modern browsers, but many don't support it and will treat the
-- cookie as non-persistent (notably IE 6, 7 and 8).
createCookieTemplate :: State sto -> C.SetCookie
createCookieTemplate state =
  -- Generate a cookie with the final session ID.
  def
    { C.setCookiePath     = Just "/"
    , C.setCookieMaxAge   = calculateMaxAge state
    , C.setCookieDomain   = Nothing
    , C.setCookieHttpOnly = getHttpOnlyCookies state
    , C.setCookieSecure   = getSecureCookies state
    }


-- | Calculate the @Max-age@ of a cookie template for the given
-- state.
--
--   * If the state asks for non-persistent sessions, the result
--   is @Nothing@.
--
--   * If no timeout is defined, the result is 10 years.
--
--   * Otherwise, the max age is set as the maximum timeout.
calculateMaxAge :: State sto -> Maybe TI.DiffTime
calculateMaxAge st = do
  guard (persistentCookies st)
  return $ maybe (60*60*24*3652) realToFrac
         $ idleTimeout st `max` absoluteTimeout st


-- | Invalidate the current session ID (and possibly more, check
-- 'ForceInvalidate').  This is useful to avoid session fixation
-- attacks (cf. <http://www.acrossecurity.com/papers/session_fixation.pdf>).
forceInvalidate :: WS.Session m Text ByteString -> ForceInvalidate -> m ()
forceInvalidate (_, insert) = insert forceInvalidateKey . B8.pack . show
