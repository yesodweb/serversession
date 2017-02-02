-- | Internal module exposing the guts of the package.  Use at
-- your own risk.  No API stability guarantees apply.
module Web.ServerSession.Frontend.Snap.Internal
  ( initServerSessionManager
  , simpleServerSessionManager
  , SnapSession(..)
  , ServerSessionManager(..)
  , currentSessionMap
  , modifyCurrentSession
  , createCookie
  , csrfKey
  , forceInvalidate
  ) where

import Control.Applicative as A
import Control.Arrow (first, second)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Web.PathPieces (toPathPiece)
import Web.ServerSession.Core

import qualified Crypto.Nonce as N
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as TI
import qualified Snap.Core as S
import qualified Snap.Snaplet as S
import qualified Snap.Snaplet.Session as S
import qualified Snap.Snaplet.Session.SessionManager as S


-- | Create a new 'ServerSessionManager' using the given 'State'.
initServerSessionManager
  :: (Storage sto, SnapSession (SessionData sto))
  => IO (State sto)
  -> S.SnapletInit b S.SessionManager
initServerSessionManager mkState =
  S.makeSnaplet "ServerSession"
                "Snaplet providing sessions via server-side storage."
                Nothing $ liftIO $ do
    gen <- N.new
    st  <- mkState
    let ssm = ServerSessionManager
          { currentSession = Nothing
          , state = st
          , cookieName = TE.encodeUtf8 $ getCookieName st
          , nonceGen = gen
          }
    return $ S.SessionManager ssm


-- | Simplified version of 'initServerSessionManager', sufficient
-- for most needs.
simpleServerSessionManager
  :: (Storage sto, SessionData sto ~ SessionMap)
  => IO sto
  -> (State sto -> State sto)
  -> S.SnapletInit b S.SessionManager
simpleServerSessionManager mkStorage opts =
  initServerSessionManager (fmap opts . createState =<< mkStorage)


----------------------------------------------------------------------


-- | Class for data types that implement the operations Snap
-- expects sessions to support.
class IsSessionData sess => SnapSession sess where
  ssInsert :: Text -> Text -> sess -> sess
  ssLookup :: Text -> sess -> Maybe Text
  ssDelete :: Text -> sess -> sess
  ssToList :: sess -> [(Text, Text)]

  ssInsertCsrf :: Text -> sess -> sess
  ssLookupCsrf :: sess -> Maybe Text

  ssForceInvalidate :: ForceInvalidate -> sess -> sess


-- | Uses 'csrfKey'.
instance SnapSession SessionMap where
  ssInsert key val = onSM (HM.insert key (TE.encodeUtf8 val))
  ssLookup key     = fmap TE.decodeUtf8 . HM.lookup key . unSessionMap
  ssDelete key     = onSM (HM.delete key)
  ssToList =
    -- Remove the CSRF key from the list as the current
    -- clientsession backend doesn't return it.
    fmap (second TE.decodeUtf8) .
    HM.toList .
    HM.delete csrfKey .
    unSessionMap

  ssInsertCsrf = ssInsert csrfKey
  ssLookupCsrf = ssLookup csrfKey

  ssForceInvalidate force = onSM (HM.insert forceInvalidateKey (B8.pack $ show force))


-- | Apply a function to a 'SessionMap'.
onSM
  :: (HM.HashMap Text ByteString -> HM.HashMap Text ByteString)
  -> (SessionMap                 -> SessionMap)
onSM f = SessionMap . f . unSessionMap


----------------------------------------------------------------------


-- | A 'S.ISessionManager' using server-side sessions.
data ServerSessionManager sto =
  ServerSessionManager
    { currentSession :: Maybe (SessionData sto, SaveSessionToken sto)
      -- ^ Field used for per-request caching of the session.
    , state :: State sto
      -- ^ The core @serversession@ state.
    , cookieName :: ByteString
      -- ^ Cache of the cookie name as bytestring.
    , nonceGen :: N.Generator
      -- ^ Nonce generator for the CSRF token.
    } deriving (Typeable)


instance ( Storage sto
         , SnapSession (SessionData sto)
         ) => S.ISessionManager (ServerSessionManager sto) where
  load ssm@ServerSessionManager { currentSession = Just _ } =
    -- Don't do anything if already loaded.  Yeah, I know this is
    -- strange, go figure.
    return ssm
  load ssm = do
    -- Get session ID from cookie.
    mcookie <- S.getCookie (cookieName ssm)
    -- Load session from storage backend.
    (data1, saveSessionToken) <-
      liftIO $ loadSession (state ssm) (S.cookieValue A.<$> mcookie)
    -- Add CSRF token if needed.
    data2 <-
      maybe
        (flip ssInsertCsrf data1 <$> N.nonce128urlT (nonceGen ssm))
        (const $ return data1)
        (ssLookupCsrf data1)
    -- Good to go!
    return ssm { currentSession = Just (data2, saveSessionToken) }

  commit ssm = do
    -- Save session data to storage backend and set the cookie.
    let Just (data_, saveSessionToken) = currentSession ssm
    msession <- liftIO $ saveSession (state ssm) saveSessionToken data_
    S.modifyResponse $ S.addResponseCookie $
      maybe
        (deleteCookie (state ssm) (cookieName ssm))
        (createCookie (state ssm) (cookieName ssm))
        msession

  reset ssm = do
    -- Reset has no defined semantics.  We invalidate the session
    -- and clear its variables, which seems to be what the
    -- current clientsession backend from the snap package does.
    csrfToken <- N.nonce128urlT (nonceGen ssm)
    let newSession =
          ssInsertCsrf      csrfToken        $
          ssForceInvalidate CurrentSessionId $
          emptySession
    return $ modifyCurrentSession (const newSession) ssm

  touch =
    -- We always touch the session (if commit is called).
    id

  insert key value = modifyCurrentSession (ssInsert key value)

  lookup key =
    -- Decoding will always succeed if the session is used only
    -- from snap.
    ssLookup key . currentSessionMap "lookup"

  delete key = modifyCurrentSession (ssDelete key)

  csrf =
    -- Guaranteed to succeed since both load and reset add a
    -- csrfKey to the session map.
    fromMaybe (error "serversession-frontend-snap/csrf: never here") .
    ssLookupCsrf . currentSessionMap "csrf"

  toList = ssToList . currentSessionMap "toList"


-- | Get the current 'SessionData' from 'currentSession' and
-- unwrap its @Just@.  If it's @Nothing@, @error@ is called.  We
-- expect 'load' to be called before any other 'ISessionManager'
-- method.
currentSessionMap :: String -> ServerSessionManager sto -> SessionData sto
currentSessionMap fn ssm = maybe (error err) fst (currentSession ssm)
  where err = "serversession-frontend-snap/" ++ fn ++
              ": currentSession is Nothing, did you call 'load'?"


-- | Modify the current session in any way.
modifyCurrentSession
  :: (SessionData sto -> SessionData sto)
  -> ServerSessionManager sto
  -> ServerSessionManager sto
modifyCurrentSession f ssm = ssm { currentSession = fmap (first f) (currentSession ssm) }


----------------------------------------------------------------------


-- | Create a cookie for the given session.
--
-- The cookie expiration is set via 'nextExpires'.  Note that
-- this is just an optimization, as the expiration is checked on
-- the server-side as well.
createCookie :: State sto -> ByteString -> Session sess -> S.Cookie
createCookie st cookieNameBS session =
  -- Generate a cookie with the final session ID.
  S.Cookie
    { S.cookieName     = cookieNameBS
    , S.cookieValue    = TE.encodeUtf8 $ toPathPiece $ sessionKey session
    , S.cookiePath     = Just "/"
    , S.cookieExpires  = cookieExpires st session
    , S.cookieDomain   = Nothing
    , S.cookieHttpOnly = getHttpOnlyCookies st
    , S.cookieSecure   = getSecureCookies st
    }


-- | Remove the session cookie from the client.  This is used
-- when 'saveSession' returns @Nothing@:
--
--   * If the user didn't have a session cookie, this cookie
--   deletion will be harmless.
--
--   * If the user had a session cookie that was invalidated,
--   this will remove the invalid cookie from the client.
-- the server-side as well.
deleteCookie :: State sto -> ByteString -> S.Cookie
deleteCookie st cookieNameBS =
  S.Cookie
    { S.cookieName     = cookieNameBS
    , S.cookieValue    = ""
    , S.cookiePath     = Just "/"
    , S.cookieExpires  = Just aLongTimeAgo
    , S.cookieDomain   = Nothing
    , S.cookieHttpOnly = getHttpOnlyCookies st
    , S.cookieSecure   = getSecureCookies st
    }
  where aLongTimeAgo = read "1970-01-01 00:00:01 UTC" :: TI.UTCTime


-- | The CSRF key is kept as a session variable like any other
-- under this key.
csrfKey :: Text
csrfKey = "_CSRF"


-- | Invalidate the current session ID (and possibly more, check
-- 'ForceInvalidate').  This is useful to avoid session fixation
-- attacks (cf. <http://www.acrossecurity.com/papers/session_fixation.pdf>).
--
-- Note that the invalidate /does not/ occur when the call to
-- this action is made!  The sessions will be invalidated when
-- the session is 'commit'ed.  This means that later calls to
-- 'forceInvalidate' on the same handler will override earlier
-- calls.
--
-- This function works by setting a session variable that is
-- checked when saving the session.  The session variable set by
-- this function is then discarded and is not persisted across
-- requests.
forceInvalidate :: ForceInvalidate -> S.Handler b S.SessionManager ()
forceInvalidate = S.setInSession forceInvalidateKey . T.pack . show
