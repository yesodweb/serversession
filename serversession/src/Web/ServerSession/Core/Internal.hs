-- | Internal module exposing the guts of the package.  Use at
-- your own risk.  No API stability guarantees apply.
module Web.ServerSession.Core.Internal
  ( SessionId(..)
  , checkSessionId
  , generateSessionId

  , SessionMap
  , Session(..)
  , Storage(..)

  , State(..)
  , createState
  , setCookieName
  , setAuthKey
  , loadSession
  , saveSession
  , SaveSessionToken(..)
  , invalidateIfNeeded
  , DecomposedSession
  , decomposeSession
  , saveSessionOnDb
  , toSessionMap
  , forceInvalidateKey
  , ForceInvalidate(..)
  ) where

import Control.Monad (guard, when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Data.Typeable (Typeable)
import Web.PathPieces (PathPiece(..))

import qualified Crypto.Nonce as N
import qualified Data.Aeson as A
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE


----------------------------------------------------------------------


-- | The ID of a session.  Always 18 bytes base64url-encoded as
-- 24 characters.
--
-- Implementation notes:
--
--   * Use 'fromPathPiece' for parsing untrusted input.
--
--   * Use 'generateSessionId' for securely generating new
--   session IDs.
newtype SessionId = S { unS :: Text }
  deriving (Eq, Ord, Show, Read, Typeable)

-- | Sanity checks input on 'fromPathPiece' (untrusted input).
instance PathPiece SessionId where
  toPathPiece = unS
  fromPathPiece = checkSessionId

instance A.FromJSON SessionId where
  parseJSON = fmap S . A.parseJSON

instance A.ToJSON SessionId where
  toJSON = A.toJSON . unS


-- | (Internal) Check that the given text is a base64url-encoded
-- representation of 18 bytes.
checkSessionId :: Text -> Maybe SessionId
checkSessionId text = do
  guard (T.length text == 24)
  let bs = TE.encodeUtf8 text
  decoded <- either (const Nothing) Just $ B64URL.decode bs
  guard (B8.length decoded == 18)
  return $ S $ T.toLower text


-- | Securely generate a new SessionId.
generateSessionId :: N.Generator -> IO SessionId
generateSessionId = fmap S . N.nonce128urlT


----------------------------------------------------------------------


-- | A session map.
--
-- This is the representation of a session used by the
-- @serversession@ family of packages, transferring data between
-- this core package and frontend packages.  Serversession
-- storage backend packages should use 'Session'.  End users
-- should use their web framework's support for sessions.
type SessionMap = M.Map Text ByteString


-- | Representation of a saved session.
--
-- This representation is used by the @serversession@ family of
-- packages, transferring data between this core package and
-- storage backend packages.  Serversession frontend packages
-- should use 'SessionMap'.  End users should use their web
-- framework's support for sessions.
data Session =
  Session
    { sessionKey :: SessionId
      -- ^ Session ID, primary key.
    , sessionAuthId :: Maybe ByteString
      -- ^ Value of 'authKey' session key, separate from the rest.
    , sessionData :: SessionMap
      -- ^ Rest of the session data.
    , sessionCreatedAt :: UTCTime
      -- ^ When this session was created.
    } deriving (Eq, Ord, Show, Typeable)


-- | A storage backend for server-side sessions.
class MonadIO (TransactionM s) => Storage s where
  -- | Monad where transactions happen for this backend.
  -- We do not require transactions to be ACID.
  type TransactionM s :: * -> *

  -- | Run a transaction on the IO monad.
  runTransactionM :: s -> TransactionM s a -> IO a

  -- | Get the session for the given session ID.
  getSession :: s -> SessionId -> TransactionM s (Maybe Session)

  -- | Delete the session with given session ID.
  deleteSession :: s -> SessionId -> TransactionM s ()

  -- | Delete all sessions of the given auth ID.
  deleteAllSessionsOfAuthId :: s -> ByteString -> TransactionM s ()

  -- | Insert a new session.
  insertSession :: s -> Session -> TransactionM s ()

  -- | Replace the contents of a session.
  replaceSession :: s -> Session -> TransactionM s ()


----------------------------------------------------------------------


-- TODO: expiration

-- TODO: do not create empty sessions

-- | The server-side session backend needs to maintain some state
-- in order to work:
--
--   * A nonce generator for the session IDs.
--
--   * A reference to the storage backend.
--
--   * The name of cookie where the session ID will be saved ('setCookieName').
--
--   * Authentication session variable ('setAuthKey').
--
--   * Idle and absolute timeouts ('setIdleTimeout' and 'setAbsoluteTimeout').
--
-- Create a new 'State' using 'createState'.
data State s =
  State
    { generator       :: !N.Generator
    , storage         :: !s
    , cookieName      :: !Text
    , authKey         :: !Text
    , idleTimeout     :: !(Maybe DiffTime)
    , absoluteTimeout :: !(Maybe DiffTime)
    } deriving (Typeable)


-- | Create a new 'State' for the server-side session backend
-- using the given storage backend.
createState :: MonadIO m => s -> m (State s)
createState sto = do
  gen <- N.new
  return State
    { generator       = gen
    , storage         = sto
    , cookieName      = "JSESSIONID"
    , authKey         = "_ID"
    , idleTimeout     = Just $ secondsToDiffTime $ 60*60*24*7  -- 7 days
    , absoluteTimeout = Just $ secondsToDiffTime $ 60*60*24*60 -- 60 days
    }


-- | Set the name of cookie where the session ID will be saved.
-- Defaults to \"JSESSIONID\", which is a generic cookie name
-- used by many frameworks thus making it harder to fingerprint
-- this implementation.
setCookieName :: Text -> State s -> State s
setCookieName val state = state { cookieName = val }


-- | Set the name of the session variable that keeps track of the
-- logged user.  Defaults to \"_ID\" (used by @yesod-auth@).
setAuthKey :: Text -> State s -> State s
setAuthKey val state = state { authKey = val }


-- | Load the session map from the storage backend.  The value of
-- the session cookie should be given as argument if present.
--
-- Returns:
--
--   * The 'SessionMap' to be used by the frontend as the current
--   session's value.
--
--   * Information to be passed back to 'saveSession' on the end
--   of the request in order to save the session.
loadSession :: Storage s => State s -> Maybe ByteString -> IO (SessionMap, SaveSessionToken)
loadSession state mcookieVal = do
  let maybeInputId = mcookieVal >>= fromPathPiece . TE.decodeUtf8
      get = runTransactionM (storage state) . getSession (storage state)
  maybeInput <- maybe (return Nothing) get maybeInputId
  let inputSessionMap = maybe M.empty (toSessionMap state) maybeInput
  return (inputSessionMap, SaveSessionToken maybeInput)


-- | Opaque token containing the necessary information for
-- 'saveSession' to save the session.
newtype SaveSessionToken = SaveSessionToken (Maybe Session)


-- | Save the session on the storage backend.  A
-- 'SaveSessionToken' given by 'loadSession' is expected besides
-- the new contents of the session.
saveSession :: Storage s => State s -> SaveSessionToken -> SessionMap -> IO SessionId
saveSession state (SaveSessionToken maybeInput) wholeOutputSessionMap =
  runTransactionM (storage state) $ do
    let decomposedSessionMap = decomposeSession state wholeOutputSessionMap
    newMaybeInput <- invalidateIfNeeded state maybeInput decomposedSessionMap
    saveSessionOnDb state newMaybeInput decomposedSessionMap


-- | Invalidates an old session ID if needed.  Returns the
-- 'Session' that should be replaced when saving the session, if any.
--
-- Currently we invalidate whenever the auth ID has changed
-- (login, logout, different user) in order to prevent session
-- fixation attacks.  We also invalidate when asked to via
-- 'forceInvalidate'.
invalidateIfNeeded
  :: Storage s
  => State s
  -> Maybe Session
  -> DecomposedSession
  -> TransactionM s (Maybe Session)
invalidateIfNeeded state maybeInput DecomposedSession {..} = do
  -- Decide which action to take.
  -- "invalidateOthers implies invalidateCurrent" should be true below.
  let inputAuthId       = sessionAuthId =<< maybeInput
      invalidateCurrent = dsForceInvalidate /= DoNotForceInvalidate || inputAuthId /= dsAuthId
      invalidateOthers  = dsForceInvalidate == AllSessionIdsOfLoggedUser && isJust dsAuthId
      whenMaybe b m f   = when b $ maybe (return ()) f m
  -- Delete current and others, as requested.
  whenMaybe invalidateCurrent maybeInput $ deleteSession (storage state) . sessionKey
  whenMaybe invalidateOthers  dsAuthId   $ deleteAllSessionsOfAuthId (storage state)
  -- Remember the input only if not invalidated.
  return $ guard (not invalidateCurrent) >> maybeInput


-- | A 'SessionMap' with its special variables taken apart.
data DecomposedSession =
  DecomposedSession
    { dsAuthId          :: !(Maybe ByteString)
    , dsForceInvalidate :: !ForceInvalidate
    , dsSessionMap      :: !SessionMap
    } deriving (Show, Typeable)


-- | Decompose a session (see 'DecomposedSession').
decomposeSession :: State s -> SessionMap -> DecomposedSession
decomposeSession state sm1 =
  let (authId, sm2) = M.updateLookupWithKey (\_ _ -> Nothing) (authKey state)    sm1
      (force,  sm3) = M.updateLookupWithKey (\_ _ -> Nothing) forceInvalidateKey sm2
  in DecomposedSession
       { dsAuthId          = authId
       , dsForceInvalidate = maybe DoNotForceInvalidate (read . B8.unpack) force
       , dsSessionMap      = sm3 }


-- | Save a session on the database.  If an old session is
-- supplied, it is replaced, otherwise a new session is
-- generated.
saveSessionOnDb
  :: Storage s
  => State s
  -> Maybe Session            -- ^ The old session, if any.
  -> DecomposedSession        -- ^ The session data to be saved.
  -> TransactionM s SessionId -- ^ The ID of the saved session.
saveSessionOnDb state maybeInput DecomposedSession {..} = do
  -- Generate properties if needed or take them from previous
  -- saved session.
  (saveToDb, key, createdAt) <-
    case maybeInput of
      Nothing -> liftIO $
        (,,) <$> return (insertSession $ storage state)
             <*> generateSessionId (generator state)
             <*> getCurrentTime
      Just Session {..} ->
        return ( replaceSession (storage state)
               , sessionKey
               , sessionCreatedAt)
  -- Save to the database.
  saveToDb $ Session key dsAuthId dsSessionMap createdAt
  return key


-- | Create a 'SessionMap' from a 'Session'.
toSessionMap :: State s -> Session -> SessionMap
toSessionMap state Session {..} =
  maybe id (M.insert $ authKey state) sessionAuthId sessionData


-- | The session key used to signal that the session ID should be
-- invalidated.
forceInvalidateKey :: Text
forceInvalidateKey = "serversession-force-invalidate"


-- | Which session IDs should be invalidated.
--
-- Note that this is not the same concept of invalidation as used
-- on J2EE.  In this context, invalidation means creating a fresh
-- session ID for this user's session and disabling the old ID.
-- Its purpose is to avoid session fixation attacks.
data ForceInvalidate =
    CurrentSessionId
    -- ^ Invalidate the current session ID.  The current session
    -- ID is automatically invalidated on login and logout
    -- (cf. 'setAuthKey').
  | AllSessionIdsOfLoggedUser
    -- ^ Invalidate all session IDs beloging to the currently
    -- logged in user.  Only the current session ID will be
    -- renewed (the only one for which a cookie can be set).
    --
    -- This is useful, for example, if the user asks to change
    -- their password.  It's also useful to provide a button to
    -- clear all other sessions.
    --
    -- If the user is not logged in, this option behaves exactly
    -- as 'CurrentSessionId' (i.e., it /does not/ invalidate the
    -- sessions of all logged out users).
    --
    -- Note that, for the purposes of
    -- 'AllSessionIdsOfLoggedUser', we consider \"logged user\"
    -- the one that is logged in at the *end* of the handler
    -- processing.  For example, if the user was logged in but
    -- the current handler logged him out, the session IDs of the
    -- user who was logged in will not be invalidated.
  | DoNotForceInvalidate
    -- ^ Do not force invalidate.  Invalidate only if
    -- automatically.  This is the default.
    deriving (Eq, Ord, Show, Read, Enum, Typeable)
