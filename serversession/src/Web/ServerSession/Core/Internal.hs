-- | Internal module exposing the guts of the package.  Use at
-- your own risk.  No API stability guarantees apply.
--
-- @UndecidableInstances@ is required in order to implement @Eq@,
-- @Ord@, @Show@, etc. on data types that have @Decomposed@
-- fields, and should be fairly safe.
module Web.ServerSession.Core.Internal
  ( SessionId(..)
  , checkSessionId
  , generateSessionId

  , AuthId
  , Session(..)
  , SessionMap(..)

  , IsSessionData(..)
  , DecomposedSession(..)

  , Storage(..)
  , StorageException(..)

  , State(..)
  , createState
  , setCookieName
  , setAuthKey
  , setIdleTimeout
  , setAbsoluteTimeout
  , setTimeoutResolution
  , setPersistentCookies
  , setHttpOnlyCookies
  , setSecureCookies
  , getCookieName
  , getHttpOnlyCookies
  , getSecureCookies

  , loadSession
  , checkExpired
  , nextExpires
  , cookieExpires
  , saveSession
  , SaveSessionToken(..)
  , invalidateIfNeeded
  , saveSessionOnDb
  , forceInvalidateKey
  , ForceInvalidate(..)
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (guard, when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock (NominalDiffTime, addUTCTime, diffUTCTime)
import Data.Typeable (Typeable)
import Web.PathPieces (PathPiece(..))

import qualified Control.Exception as E
import qualified Crypto.Nonce as N
import qualified Data.Aeson as A
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE


----------------------------------------------------------------------


-- | The ID of a session.  Always 18 bytes base64url-encoded as
-- 24 characters.  The @sess@ type variable is a phantom type for
-- the session data type this session ID points to.
--
-- Implementation notes:
--
--   * Use 'fromPathPiece' for parsing untrusted input.
--
--   * Use 'generateSessionId' for securely generating new
--   session IDs.
newtype SessionId sess = S { unS :: Text }
  deriving (Eq, Ord, Show, Read, Typeable)

-- | Sanity checks input on 'fromPathPiece' (untrusted input).
instance PathPiece (SessionId sess) where
  toPathPiece = unS
  fromPathPiece = checkSessionId

instance A.FromJSON (SessionId sess) where
  parseJSON = fmap S . A.parseJSON

instance A.ToJSON (SessionId sess) where
  toJSON = A.toJSON . unS


-- | (Internal) Check that the given text is a base64url-encoded
-- representation of 18 bytes.
checkSessionId :: Text -> Maybe (SessionId sess)
checkSessionId text = do
  guard (T.length text == 24)
  let bs = TE.encodeUtf8 text
  decoded <- either (const Nothing) Just $ B64URL.decode bs
  guard (B8.length decoded == 18)
  return $ S text


-- | Securely generate a new SessionId.
generateSessionId :: N.Generator -> IO (SessionId sess)
generateSessionId = fmap S . N.nonce128urlT


----------------------------------------------------------------------


-- | Value of the 'authKey' session key.
type AuthId = ByteString


-- | Representation of a saved session.
--
-- This representation is used by the @serversession@ family of
-- packages, transferring data between this core package and
-- storage backend packages.  The @sess@ type variable describes
-- the session data type.
data Session sess =
  Session
    { sessionKey :: SessionId sess
      -- ^ Session ID, primary key.
    , sessionAuthId :: Maybe AuthId
      -- ^ Value of 'authKey' session key, separate from the rest.
    , sessionData :: Decomposed sess
      -- ^ Rest of the session data.
    , sessionCreatedAt :: UTCTime
      -- ^ When this session was created.
    , sessionAccessedAt :: UTCTime
      -- ^ When this session was last accessed.
    } deriving (Typeable)

deriving instance Eq   (Decomposed sess) => Eq   (Session sess)
deriving instance Ord  (Decomposed sess) => Ord  (Session sess)
deriving instance Show (Decomposed sess) => Show (Session sess)


-- | A @newtype@ for a common session map.
--
-- This is a common representation of a session.  Although
-- @serversession@ has generalized session data types, you can
-- use this one if you don't want to worry about it.  We strive
-- to support this session data type on all frontends and storage
-- backends.
newtype SessionMap =
  SessionMap { unSessionMap :: M.Map Text ByteString }
  deriving (Eq, Ord, Show, Read, Typeable)


----------------------------------------------------------------------


-- | Class for data types to be used as session data
-- (cf. 'sessionData', 'SessionData').
--
-- The @Show@ constrain is needed for 'StorageException'.
class ( Show (Decomposed sess)
      , Typeable (Decomposed sess)
      , Typeable sess
      ) => IsSessionData sess where
  -- | The type of the session data after being decomposed.  This
  -- may be the same as @sess@.
  type Decomposed sess :: *

  -- | Empty session data.
  emptySession :: sess

  -- | Decompose session data into:
  --
  --   * The auth ID of the logged in user (cf. 'setAuthKey',
  --   'dsAuthId').
  --
  --   * If the session is being forced to be invalidated
  --   (cf. 'forceInvalidateKey', 'ForceInvalidate').
  --
  --   * The rest of the session data (cf. 'Decomposed').
  decomposeSession
    :: Text                   -- ^ The auth key (cf. 'setAuthKey').
    -> sess                   -- ^ Session data to be decomposed.
    -> DecomposedSession sess -- ^ Decomposed session data.

  -- | Recompose a decomposed session again into a proper @sess@.
  recomposeSession
    :: Text                   -- ^ The auth key (cf. 'setAuthKey').
    -> Maybe AuthId           -- ^ The @AuthId@, if any.
    -> Decomposed sess        -- ^ Decomposed session data to be recomposed.
    -> sess                   -- ^ Recomposed session data.

  -- | Returns @True@ when both session datas are to be
  -- considered the same.
  --
  -- This is used to optimize storage calls
  -- (cf. 'setTimeoutResolution').  Always returning @False@ will
  -- disable the optimization but won't have any other adverse
  -- effects.
  --
  -- For data types implementing 'Eq', this is usually a good
  -- implementation:
  --
  -- @
  -- isSameDecomposed _ = (==)
  -- @
  isSameDecomposed :: proxy sess -> Decomposed sess -> Decomposed sess -> Bool

  -- | Returns @True@ if the decomposed session data is to be
  -- considered @empty@.
  --
  -- This is used to avoid storing empty session data if at all
  -- possible.  Always returning @False@ will disable the
  -- optimization but won't have any other adverse effects.
  isDecomposedEmpty :: proxy sess -> Decomposed sess -> Bool


-- | A 'SessionMap' decomposes into a 'SessionMap' minus the keys
-- that were removed.  The auth key is added back when
-- recomposing.
instance IsSessionData SessionMap where
  type Decomposed SessionMap = SessionMap

  emptySession = SessionMap M.empty

  isSameDecomposed _ = (==)

  decomposeSession authKey_ (SessionMap sm1) =
    let (authId, sm2) = M.updateLookupWithKey (\_ _ -> Nothing) authKey_           sm1
        (force,  sm3) = M.updateLookupWithKey (\_ _ -> Nothing) forceInvalidateKey sm2
    in DecomposedSession
         { dsAuthId          = authId
         , dsForceInvalidate = maybe DoNotForceInvalidate (read . B8.unpack) force
         , dsDecomposed      = SessionMap sm3 }

  recomposeSession authKey_ mauthId (SessionMap sm) =
    SessionMap $ maybe id (M.insert authKey_) mauthId sm

  isDecomposedEmpty _ = M.null . unSessionMap


-- | A session data type @sess@ with its special variables taken apart.
data DecomposedSession sess =
  DecomposedSession
    { dsAuthId          :: !(Maybe ByteString)
    , dsForceInvalidate :: !ForceInvalidate
    , dsDecomposed      :: !(Decomposed sess)
    } deriving (Typeable)

deriving instance Eq   (Decomposed sess) => Eq   (DecomposedSession sess)
deriving instance Ord  (Decomposed sess) => Ord  (DecomposedSession sess)
deriving instance Show (Decomposed sess) => Show (DecomposedSession sess)


----------------------------------------------------------------------


-- | A storage backend @sto@ for server-side sessions.  The
-- @sess@ session data type and\/or its 'Decomposed' version may
-- be constrained depending on the storage backend capabilities.
class ( Typeable sto
      , MonadIO (TransactionM sto)
      , IsSessionData (SessionData sto)
      ) => Storage sto where
  -- | The session data type used by this storage.
  type SessionData sto :: *

  -- | Monad where transactions happen for this backend.
  -- We do not require transactions to be ACID.
  type TransactionM sto :: * -> *

  -- | Run a transaction on the IO monad.
  runTransactionM :: sto -> TransactionM sto a -> IO a

  -- | Get the session for the given session ID.  Returns
  -- @Nothing@ if the session is not found.
  getSession
    :: sto
    -> SessionId (SessionData sto)
    -> TransactionM sto (Maybe (Session (SessionData sto)))

  -- | Delete the session with given session ID.  Does not do
  -- anything if the session is not found.
  deleteSession :: sto -> SessionId (SessionData sto) -> TransactionM sto ()

  -- | Delete all sessions of the given auth ID.  Does not do
  -- anything if there are no sessions of the given auth ID.
  deleteAllSessionsOfAuthId :: sto -> AuthId -> TransactionM sto ()

  -- | Insert a new session.  Throws 'SessionAlreadyExists' if
  -- there already exists a session with the same session ID (we
  -- only call this method after generating a fresh session ID).
  insertSession :: sto -> Session (SessionData sto) -> TransactionM sto ()

  -- | Replace the contents of a session.  Throws
  -- 'SessionDoesNotExist' if there is no session with the given
  -- session ID (we only call this method when updating a session
  -- that is known to exist).
  --
  -- It is possible to have concurrent requests using the same
  -- session ID such that:
  --
  -- @
  -- request 1: loadSession
  --                        request 2: loadSession
  --                        request 2: forceInvalidate
  --                        request 2: saveSession
  -- request 1: saveSession
  -- @
  --
  -- The request 2's call to 'saveSession' will have called
  -- 'deleteSession' as invalidation was forced.  However,
  -- request 1 has no idea and will try to @replaceSession@.  The
  -- following behaviors are possible:
  --
  --   1. Make @replaceSession@ insert the session again.
  --   However, this will undo the invalidation of request 2.  As
  --   invalidations are done for security reasons, this is a bad
  --   idea.
  --
  --   2. Make @replaceSession@ silently discard the session.
  --   The reasoning is that, as the session was going to be
  --   invalidated if request 2 came after request 1, we can
  --   discard its contents.  However, we can't be sure that
  --   request 2 would have had the same effect if it had seen
  --   the session changes made by request 1 (and vice versa).
  --
  --   3. Make @replaceSession@ throw an error.  This error is
  --   going to be unrecoverable since usually the session
  --   processing is done at the end of the request processing by
  --   the web framework, thus leading to a 500 Internal Server
  --   Error.  However, this signals to the caller that something
  --   went wrong, which is correct.
  --
  -- Most of the time this discussion does not matter.
  -- Invalidations usually occur at times where only one request
  -- is flying.
  replaceSession :: sto -> Session (SessionData sto) -> TransactionM sto ()


-- | Common exceptions that may be thrown by any storage.
data StorageException sto =
    -- | Exception thrown by 'insertSession' whenever a session
    -- with same ID already exists.
    SessionAlreadyExists
      { seExistingSession :: Session (SessionData sto)
      , seNewSession      :: Session (SessionData sto) }
    -- | Exception thrown by 'replaceSession' whenever trying to
    -- replace a session that is not present on the storage.
  | SessionDoesNotExist
      { seNewSession      :: Session (SessionData sto) }
    deriving (Typeable)

deriving instance Eq   (Decomposed (SessionData sto)) => Eq   (StorageException sto)
deriving instance Ord  (Decomposed (SessionData sto)) => Ord  (StorageException sto)
deriving instance Show (Decomposed (SessionData sto)) => Show (StorageException sto)

instance Storage sto => E.Exception (StorageException sto) where


----------------------------------------------------------------------


-- TODO: delete expired sessions.

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
--   * Timeout resolution ('setTimeoutResolution').
--
--   * Whether cookies should be persistent
--   ('setPersistentCookies'), HTTP-only ('setHTTPOnlyCookies')
--   and/or secure ('setSecureCookies').
--
-- Create a new 'State' using 'createState'.
data State sto =
  State
    { generator         :: !N.Generator
    , storage           :: !sto
    , cookieName        :: !Text
    , authKey           :: !Text
    , idleTimeout       :: !(Maybe NominalDiffTime)
    , absoluteTimeout   :: !(Maybe NominalDiffTime)
    , timeoutResolution :: !(Maybe NominalDiffTime)
    , persistentCookies :: !Bool
    , httpOnlyCookies   :: !Bool
    , secureCookies     :: !Bool
    } deriving (Typeable)


-- | Create a new 'State' for the server-side session backend
-- using the given storage backend.
createState :: MonadIO m => sto -> m (State sto)
createState sto = do
  gen <- N.new
  return State
    { generator         = gen
    , storage           = sto
    , cookieName        = "JSESSIONID"
    , authKey           = "_ID"
    , idleTimeout       = Just $ 60*60*24*7  -- 7 days
    , absoluteTimeout   = Just $ 60*60*24*60 -- 60 days
    , timeoutResolution = Just $ 60*10       -- 10 minutes
    , persistentCookies = True
    , httpOnlyCookies   = True
    , secureCookies     = False
    }


-- | Set the name of cookie where the session ID will be saved.
-- Defaults to \"JSESSIONID\", which is a generic cookie name
-- used by many frameworks thus making it harder to fingerprint
-- this implementation.
setCookieName :: Text -> State sto -> State sto
setCookieName val state = state { cookieName = val }


-- | Set the name of the session variable that keeps track of the
-- logged user.
--
-- This setting is used by session data types that are
-- @Map@-alike, using a @lookup@ function.  However, the
-- 'IsSessionData' instance of a session data type may choose not
-- to use it.  For example, if you implemented a custom data
-- type, you could return the @AuthId@ without needing a lookup.
--
-- Defaults to \"_ID\" (used by @yesod-auth@).
setAuthKey :: Text -> State sto -> State sto
setAuthKey val state = state { authKey = val }


-- | Set the idle timeout for all sessions.  This is used both on
-- the client side (by setting the cookie expires fields) and on
-- the server side (the idle timeout is enforced even if the
-- cookie expiration is ignored).  Setting to @Nothing@ removes
-- the idle timeout entirely.
--
-- \"[The idle timemout] defines the amount of time a session
-- will remain active in case there is no activity in the
-- session, closing and invalidating the session upon the defined
-- idle period since the last HTTP request received by the web
-- application for a given session ID.\"
-- (<https://www.owasp.org/index.php/Session_Management_Cheat_Sheet#Idle_Timeout Source>)
--
-- Defaults to 7 days.
setIdleTimeout :: Maybe NominalDiffTime -> State sto -> State sto
setIdleTimeout (Just d) _ | d <= 0 = error "serversession/setIdleTimeout: Timeout should be positive."
setIdleTimeout val state = state { idleTimeout = val }


-- | Set the absolute timeout for all sessions.  This is used both on
-- the client side (by setting the cookie expires fields) and on
-- the server side (the absolute timeout is enforced even if the
-- cookie expiration is ignored).  Setting to @Nothing@ removes
-- the absolute timeout entirely.
--
-- \"[The absolute timeout] defines the maximum amount of time a
-- session can be active, closing and invalidating the session
-- upon the defined absolute period since the given session was
-- initially created by the web application. After invalidating
-- the session, the user is forced to (re)authenticate again in
-- the web application and establish a new session.\"
-- (<https://www.owasp.org/index.php/Session_Management_Cheat_Sheet#Absolute_Timeout Source>)
--
-- Defaults to 60 days.
setAbsoluteTimeout :: Maybe NominalDiffTime -> State sto -> State sto
setAbsoluteTimeout (Just d) _ | d <= 0 = error "serversession/setAbsoluteTimeout: Timeout should be positive."
setAbsoluteTimeout val state = state { absoluteTimeout = val }


-- | Set the timeout resolution.
--
-- We need to save both the creation and last access times on
-- sessions in order to implement idle and absolute timeouts.
-- This means that we have to save the updated session on the
-- storage backend even if the request didn't change any session
-- variable, if only to update the last access time.
--
-- This setting provides an optimization where the session is not
-- updated on the storage backend provided that:
--
--   * No session variables were changed.
--
--   * The difference between the /current/ time and the last
--   /saved/ access time is less than the timeout resolution.
--
-- For example, with a timeout resolution of 1 minute, every
-- request that does not change the session variables within 1
-- minute of the last update will not generate any updates on the
-- storage backend.
--
-- If the timeout resolution is @Nothing@, then this optimization
-- becomes disabled and the session will always be updated.
--
-- Defaults to 10 minutes.
setTimeoutResolution :: Maybe NominalDiffTime -> State sto -> State sto
setTimeoutResolution (Just d) _ | d <= 0 = error "serversession/setTimeoutResolution: Resolution should be positive."
setTimeoutResolution val state = state { timeoutResolution = val }

-- | Set whether by default cookies should be persistent (@True@) or
-- non-persistent (@False@).  Persistent cookies are saved across
-- browser sessions.  Non-persistent cookies are discarded when
-- the browser is closed.
--
-- If you set cookies to be persistent and do not define any
-- timeouts ('setIdleTimeout' or 'setAbsoluteTimeout'), then the
-- cookie is set to expire in 10 years.
--
-- Defaults to @True@.
setPersistentCookies :: Bool -> State sto -> State sto
setPersistentCookies val state = state { persistentCookies = val }


-- | Set whether cookies should be HTTP-only (@True@) or not
-- (@False@).  Cookies marked as HTTP-only (\"HttpOnly\") are not
-- accessible from client-side scripting languages such as
-- JavaScript, thus preventing a large class of XSS attacks.
-- It's highly recommended to set this attribute to @True@.
--
-- Defaults to @True@.
setHttpOnlyCookies :: Bool -> State sto -> State sto
setHttpOnlyCookies val state = state { httpOnlyCookies = val }


-- | Set whether cookies should be mared \"Secure\" (@True@) or not
-- (@False@).  Cookies marked as \"Secure\" are not sent via
-- plain HTTP connections, only via HTTPS connections.  It's
-- highly recommended to set this attribute to @True@.  However,
-- since many sites do not operate over HTTPS, the default is
-- @False@.
--
-- Defaults to @False@.
setSecureCookies :: Bool -> State sto -> State sto
setSecureCookies val state = state { secureCookies = val }


-- | Cf. 'setCookieName'.
getCookieName :: State sto -> Text
getCookieName = cookieName


-- | Cf. 'setHttpOnlyCookies'.
getHttpOnlyCookies :: State sto -> Bool
getHttpOnlyCookies = httpOnlyCookies


-- | Cf. 'setSecureCookies'.
getSecureCookies :: State sto -> Bool
getSecureCookies = secureCookies


----------------------------------------------------------------------


-- | Load the session map from the storage backend.  The value of
-- the session cookie should be given as argument if present.
--
-- Returns:
--
--   * The session data @sess@ to be used by the frontend as the
--   current session's value.
--
--   * Information to be passed back to 'saveSession' on the end
--   of the request in order to save the session.
loadSession
  :: Storage sto
  => State sto
  -> Maybe ByteString
  -> IO (SessionData sto, SaveSessionToken sto)
loadSession state mcookieVal = do
  now <- getCurrentTime
  let maybeInputId = mcookieVal >>= fromPathPiece . TE.decodeUtf8
      get          = runTransactionM (storage state) . getSession (storage state)
      checkedGet   = fmap (>>= checkExpired now state) . get
  maybeInput <- maybe (return Nothing) checkedGet maybeInputId
  let inputData =
        maybe
          emptySession
          (\s -> recomposeSession (authKey state) (sessionAuthId s) (sessionData s))
          maybeInput
  return (inputData, SaveSessionToken maybeInput now)


-- | Check if a session @s@ has expired.  Returns the @Just s@ if
-- not expired, or @Nothing@ if expired.
checkExpired :: UTCTime {-^ Now. -} -> State sto -> Session sess -> Maybe (Session sess)
checkExpired now state session =
  let expired = maybe False (< now) (nextExpires state session)
  in guard (not expired) >> return session


-- | Calculate the next point in time where the given session
-- will expire assuming that it sees no activity until then.
-- Returns @Nothing@ iff the state does not have any expirations
-- set to @Just@.
nextExpires :: State sto -> Session sess -> Maybe UTCTime
nextExpires State {..} Session {..} =
  let viaIdle     = flip addUTCTime sessionAccessedAt <$> idleTimeout
      viaAbsolute = flip addUTCTime sessionCreatedAt  <$> absoluteTimeout
      minimum' [] = Nothing
      minimum' xs = Just $ minimum xs
  in minimum' $ catMaybes [viaIdle, viaAbsolute]


-- | Calculate the date that should be used for the cookie's
-- \"Expires\" field.
cookieExpires :: State sto -> Session sess -> Maybe UTCTime
cookieExpires State {..} _ | not persistentCookies = Nothing
cookieExpires state session =
  Just $ fromMaybe tenYearsFromNow $ nextExpires state session
  where tenYearsFromNow = addUTCTime (60*60*24*3652) now
        now = sessionAccessedAt session -- :)


-- | Opaque token containing the necessary information for
-- 'saveSession' to save the session.
data SaveSessionToken sto =
  SaveSessionToken (Maybe (Session (SessionData sto))) UTCTime
  deriving (Typeable)

deriving instance Eq   (Decomposed (SessionData sto)) => Eq   (SaveSessionToken sto)
deriving instance Ord  (Decomposed (SessionData sto)) => Ord  (SaveSessionToken sto)
deriving instance Show (Decomposed (SessionData sto)) => Show (SaveSessionToken sto)


-- | Save the session on the storage backend.  A
-- 'SaveSessionToken' given by 'loadSession' is expected besides
-- the new contents of the session.
--
-- Returns @Nothing@ if the session was empty and didn't need to
-- be saved.  Note that this does /not/ necessarily means that
-- nothing was done.  If you ask for a session to be invalidated
-- and clear every other sesssion variable, then 'saveSession'
-- will invalidate the older session but will avoid creating a
-- new, empty one.
saveSession
  :: Storage sto
  => State sto
  -> SaveSessionToken sto
  -> SessionData sto
  -> IO (Maybe (Session (SessionData sto)))
saveSession state (SaveSessionToken maybeInput now) outputData =
  runTransactionM (storage state) $ do
    let outputDecomp = decomposeSession (authKey state) outputData
    newMaybeInput <- invalidateIfNeeded state maybeInput outputDecomp
    saveSessionOnDb state now newMaybeInput outputDecomp


-- | Invalidates an old session ID if needed.  Returns the
-- 'Session' that should be replaced when saving the session, if any.
--
-- Currently we invalidate whenever the auth ID has changed
-- (login, logout, different user) in order to prevent session
-- fixation attacks.  We also invalidate when asked to via
-- 'forceInvalidate'.
invalidateIfNeeded
  :: Storage sto
  => State sto
  -> Maybe (Session (SessionData sto))
  -> DecomposedSession (SessionData sto)
  -> TransactionM sto (Maybe (Session (SessionData sto)))
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


-- | Save a session on the database.  If an old session is
-- supplied, it is replaced, otherwise a new session is
-- generated.  If the session is empty, it is not saved and
-- @Nothing@ is returned.  If the timeout resolution optimization
-- is applied (cf. 'setTimeoutResolution'), the old session is
-- returned and no update is made.
saveSessionOnDb
  :: forall sto. Storage sto
  => State sto
  -> UTCTime                                            -- ^ Now.
  -> Maybe (Session (SessionData sto))                  -- ^ The old session, if any.
  -> DecomposedSession (SessionData sto)                -- ^ The session data to be saved.
  -> TransactionM sto (Maybe (Session (SessionData sto))) -- ^ Copy of saved session.
saveSessionOnDb _ _ Nothing (DecomposedSession Nothing _ m)
  -- Return Nothing without doing anything whenever the session
  -- is empty (including auth ID) and there was no prior session.
  | isDecomposedEmpty proxy m = return Nothing
    where
      proxy :: Maybe (SessionData sto)
      proxy = Nothing
saveSessionOnDb State { timeoutResolution = Just res } now (Just old) (DecomposedSession authId _ newSession)
  -- If the data is the same and the old access time is within
  -- the timeout resolution, just return the old session without
  -- doing anything else.
  | sessionAuthId old == authId &&
    isSameDecomposed proxy (sessionData old) newSession &&
    abs (diffUTCTime now (sessionAccessedAt old)) < res =
      return (Just old)
    where
      proxy :: Maybe (SessionData sto)
      proxy = Nothing
saveSessionOnDb state now maybeInput DecomposedSession {..} = do
  -- Generate properties if needed or take them from previous
  -- saved session.
  (saveToDb, key, createdAt) <-
    case maybeInput of
      Nothing -> liftIO $
        (,,) <$> return (insertSession $ storage state)
             <*> generateSessionId (generator state)
             <*> return now
      Just Session {..} ->
        return ( replaceSession (storage state)
               , sessionKey
               , sessionCreatedAt)
  -- Save to the database.
  let session = Session
        { sessionKey        = key
        , sessionAuthId     = dsAuthId
        , sessionData       = dsDecomposed
        , sessionCreatedAt  = createdAt
        , sessionAccessedAt = now
        }
  saveToDb session
  return (Just session)


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
    deriving (Eq, Ord, Show, Read, Bounded, Enum, Typeable)
