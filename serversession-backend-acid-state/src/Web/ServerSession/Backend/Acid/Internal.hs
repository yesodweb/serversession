{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Internal module exposing the guts of the package.  Use at
-- your own risk.  No API stability guarantees apply.
module Web.ServerSession.Backend.Acid.Internal
  ( SessionIdToSession
  , AuthIdToSessionId
  , ServerSessionAcidState(..)
  , emptyState
  , removeSessionFromAuthId
  , insertSessionForAuthId

  , getSession
  , deleteSession
  , deleteAllSessionsOfAuthId
  , insertSession
  , replaceSession

  , GetSession
  , DeleteSession
  , DeleteAllSessionsOfAuthId
  , InsertSession
  , ReplaceSession

  , AcidStorage(..)
  ) where

import Control.Applicative as A
import Control.Monad.Reader (ask)
import Control.Monad.State (get, modify, put)
import Data.Acid
import Data.Acid.Advanced
import Data.SafeCopy
import Data.Typeable (Typeable)

import qualified Control.Exception as E
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Web.ServerSession.Core as SS
import qualified Web.ServerSession.Core.Internal as SSI


----------------------------------------------------------------------


-- | Map from session IDs to sessions.  The most important map,
-- allowing us efficient access to a session given its ID.
type SessionIdToSession sess = HM.HashMap (SS.SessionId sess) (SS.Session sess)


-- | Map from auth IDs to session IDs.  Allow us to invalidate
-- all sessions of given user without having to iterate through
-- the whole 'SessionIdToSession' map.
type AuthIdToSessionId sess = HM.HashMap SS.AuthId (S.Set (SS.SessionId sess))


-- | The current sessions.
--
-- Besides the obvious map from session IDs to sessions, we also
-- maintain a map of auth IDs to session IDs.  This allow us to
-- quickly invalidate all sessions of a given user.
data ServerSessionAcidState sess =
  ServerSessionAcidState
    { sessionIdToSession :: !(SessionIdToSession sess)
    , authIdToSessionId  :: !(AuthIdToSessionId sess)
    } deriving (Typeable)


-- | Empty 'ServerSessionAcidState' used to bootstrap the 'AcidState'.
emptyState :: ServerSessionAcidState sess
emptyState = ServerSessionAcidState HM.empty HM.empty


-- | Remove the given 'SessionId' from the set of the given
-- 'AuthId' on the map.  Does not do anything if no 'AuthId' is
-- provided.
removeSessionFromAuthId
  :: SS.SessionId sess
  -> Maybe SS.AuthId
  -> AuthIdToSessionId sess
  -> AuthIdToSessionId sess
removeSessionFromAuthId sid = maybe id removeIt
  where
    removeIt authId aits
      | S.null newSet = HM.delete authId aits
      | otherwise     = HM.insert authId newSet aits
      where newSet = maybe S.empty (S.delete sid) (HM.lookup authId aits)


-- | Insert the given session ID as being part of the given auth
-- ID.  Conceptually the opposite of 'removeSessionFromAuthId'.
-- Does not do anything if no 'AuthId' is provided.
insertSessionForAuthId
  :: SS.SessionId sess
  -> Maybe SS.AuthId
  -> AuthIdToSessionId sess
  -> AuthIdToSessionId sess
insertSessionForAuthId sid = maybe id (flip (HM.insertWith S.union) (S.singleton sid))


----------------------------------------------------------------------


-- | We can't @deriveSafeCopy 0 'base ''SS.SessionMap@ because
-- @safeCopy@ doesn't contain instances for @HashMap@ as of now.
instance SafeCopy SS.SessionMap where
  putCopy = contain . safePut . HM.toList . SS.unSessionMap
  getCopy = contain $ SS.SessionMap . HM.fromList A.<$> safeGet


-- | We can't @deriveSafeCopy 0 'base ''SS.SessionId@ as
-- otherwise we'd require an unneeded @SafeCopy sess@.
instance SafeCopy (SS.SessionId sess) where
  putCopy = contain . safePut . SSI.unS
  getCopy = contain $ SSI.S <$> safeGet


-- | We can't @deriveSafeCopy 0 'base ''SS.Session@ due to the
-- required context.
instance SafeCopy (SS.Decomposed sess) => SafeCopy (SS.Session sess) where
  putCopy (SS.Session key authId data_ createdAt accessedAt) = contain $ do
    put_t <- getSafePut
    safePut key
    safePut authId
    safePut data_
    put_t createdAt
    put_t accessedAt
  getCopy = contain $ do
    get_t <- getSafeGet
    SS.Session
      <$> safeGet
      <*> safeGet
      <*> safeGet
      <*> get_t
      <*> get_t


-- | We can't @deriveSafeCopy 0 'base ''ServerSessionAcidState@ due
-- to the required context.
instance SafeCopy (SS.Decomposed sess) => SafeCopy (ServerSessionAcidState sess) where
  putCopy (ServerSessionAcidState sits aits) = contain $ do
    safePut (HM.toList sits)
    safePut (HM.toList aits)
  getCopy = contain $
    ServerSessionAcidState
      <$> (HM.fromList <$> safeGet)
      <*> (HM.fromList <$> safeGet)


----------------------------------------------------------------------


-- | Get the session for the given session ID.
getSession
  :: SS.Storage (AcidStorage sess)
  => SS.SessionId sess
  -> Query (ServerSessionAcidState sess) (Maybe (SS.Session sess))
getSession sid = HM.lookup sid . sessionIdToSession <$> ask


-- | Delete the session with given session ID.
deleteSession
  :: SS.Storage (AcidStorage sess)
  => SS.SessionId sess
  -> Update (ServerSessionAcidState sess) ()
deleteSession sid =
  modify $ \state ->
    let oldSession            = HM.lookup sid (sessionIdToSession state)
        newSessionIdToSession = HM.delete sid (sessionIdToSession state)
        newAuthIdToSessionId  = removeSessionFromAuthId sid mauthId $ authIdToSessionId state
          where mauthId = oldSession >>= SS.sessionAuthId
    in ServerSessionAcidState newSessionIdToSession newAuthIdToSessionId


-- | Delete all sessions of the given auth ID.
deleteAllSessionsOfAuthId
  :: SS.Storage (AcidStorage sess)
  => SS.AuthId
  -> Update (ServerSessionAcidState sess) ()
deleteAllSessionsOfAuthId authId =
  modify $ \state ->
    let sessionIds            = HM.lookup authId (authIdToSessionId state)
        newAuthIdToSessionId  = HM.delete authId (authIdToSessionId state)
        newSessionIdToSession = maybe id removeSession sessionIds $ sessionIdToSession state
          where removeSession = flip HM.difference . HM.fromList . map (flip (,) ()) . S.toList
    in ServerSessionAcidState newSessionIdToSession newAuthIdToSessionId


-- | Insert a new session.
insertSession
  :: SS.Storage (AcidStorage sess)
  => SS.Session sess
  -> Update (ServerSessionAcidState sess) ()
insertSession session = do
  let insertSess sits =
        case HM.lookup sid sits of
          Nothing  -> HM.insert sid session sits
          Just old -> throwAS $ SS.SessionAlreadyExists old session
      insertAuth = insertSessionForAuthId sid (SS.sessionAuthId session)
      sid        = SS.sessionKey session
  modify $ \state ->
    ServerSessionAcidState
      (insertSess $ sessionIdToSession state)
      (insertAuth $ authIdToSessionId  state)


-- | Replace the contents of a session.
replaceSession
  :: SS.Storage (AcidStorage sess)
  => SS.Session sess
  -> Update (ServerSessionAcidState sess) ()
replaceSession session = do
  -- Check that the old session exists while replacing it.
  ServerSessionAcidState sits aits <- get
  let sid = SS.sessionKey session
  case HM.lookup sid sits of
    Nothing -> throwAS $ SS.SessionDoesNotExist session
    Just oldSession -> do
      -- Remove/insert the old auth ID from the map if needed.
      let modAits | oldAuthId == newAuthId = id
                  | otherwise = insertSessionForAuthId sid newAuthId
                              . removeSessionFromAuthId sid oldAuthId
            where oldAuthId = SS.sessionAuthId oldSession
                  newAuthId = SS.sessionAuthId session
          aits' = modAits aits
          sits' = HM.insert sid session sits
      -- Put modified state in place
      put (ServerSessionAcidState sits' aits')


-- | Specialization of 'E.throw' for 'AcidStorage'.
throwAS
  :: SS.Storage (AcidStorage sess)
  => SS.StorageException (AcidStorage sess)
  -> a
throwAS = E.throw


----------------------------------------------------------------------


-- | Session storage backend using @acid-state@.
newtype AcidStorage sess =
  AcidStorage
    { acidState :: AcidState (ServerSessionAcidState sess)
      -- ^ Open 'AcidState' of server sessions.
    } deriving (Typeable)


-- | We do not provide any ACID guarantees for different actions
-- running inside the same @TransactionM AcidStorage@.
instance ( SS.IsSessionData sess
         , SafeCopy sess
         , SafeCopy (SS.Decomposed sess)
         ) => SS.Storage (AcidStorage sess) where
  type SessionData  (AcidStorage sess) = sess
  type TransactionM (AcidStorage sess) = IO
  runTransactionM = const id
  getSession                (AcidStorage s) = query  s . GetSession
  deleteSession             (AcidStorage s) = update s . DeleteSession
  deleteAllSessionsOfAuthId (AcidStorage s) = update s . DeleteAllSessionsOfAuthId
  insertSession             (AcidStorage s) = update s . InsertSession
  replaceSession            (AcidStorage s) = update s . ReplaceSession


----------------------------------------------------------------------

-- makeAcidic can't handle type variables, so we have to do
-- everything by hand. :(

data GetSession sess = GetSession (SS.SessionId sess) deriving (Typeable)
data DeleteSession sess = DeleteSession (SS.SessionId sess) deriving (Typeable)
data DeleteAllSessionsOfAuthId sess = DeleteAllSessionsOfAuthId SS.AuthId deriving (Typeable)
data InsertSession sess = InsertSession (SS.Session sess) deriving (Typeable)
data ReplaceSession sess = ReplaceSession (SS.Session sess) deriving (Typeable)

instance SafeCopy (GetSession sess) where
  putCopy (GetSession v) = contain $ safePut v
  getCopy = contain $ GetSession <$> safeGet

instance SafeCopy (DeleteSession sess) where
  putCopy (DeleteSession v) = contain $ safePut v
  getCopy = contain $ DeleteSession <$> safeGet

instance SafeCopy (DeleteAllSessionsOfAuthId sess) where
  putCopy (DeleteAllSessionsOfAuthId v) = contain $ safePut v
  getCopy = contain $ DeleteAllSessionsOfAuthId <$> safeGet

instance SafeCopy (SS.Decomposed sess) => SafeCopy (InsertSession sess) where
  putCopy (InsertSession v) = contain $ safePut v
  getCopy = contain $ InsertSession <$> safeGet

instance SafeCopy (SS.Decomposed sess) => SafeCopy (ReplaceSession sess) where
  putCopy (ReplaceSession v) = contain $ safePut v
  getCopy = contain $ ReplaceSession <$> safeGet

type AcidContext sess =
  ( SS.IsSessionData sess
  , SafeCopy sess
  , SafeCopy (SS.Decomposed sess) )

instance AcidContext sess => QueryEvent  (GetSession sess)
instance AcidContext sess => UpdateEvent (DeleteSession sess)
instance AcidContext sess => UpdateEvent (DeleteAllSessionsOfAuthId sess)
instance AcidContext sess => UpdateEvent (InsertSession sess)
instance AcidContext sess => UpdateEvent (ReplaceSession sess)

instance AcidContext sess => Method (GetSession sess) where
  type MethodResult (GetSession sess) = Maybe (SS.Session sess)
  type MethodState (GetSession sess) = ServerSessionAcidState sess
instance AcidContext sess => Method (DeleteSession sess) where
  type MethodResult (DeleteSession sess) = ()
  type MethodState (DeleteSession sess) = ServerSessionAcidState sess
instance AcidContext sess => Method (DeleteAllSessionsOfAuthId sess) where
  type MethodResult (DeleteAllSessionsOfAuthId sess) = ()
  type MethodState (DeleteAllSessionsOfAuthId sess) = ServerSessionAcidState sess
instance AcidContext sess => Method (InsertSession sess) where
  type MethodResult (InsertSession sess) = ()
  type MethodState (InsertSession sess) = ServerSessionAcidState sess
instance AcidContext sess => Method (ReplaceSession sess) where
  type MethodResult (ReplaceSession sess) = ()
  type MethodState (ReplaceSession sess) = ServerSessionAcidState sess

instance AcidContext sess => IsAcidic (ServerSessionAcidState sess) where
  acidEvents =
    [ QueryEvent  $ \(GetSession sid)                   -> getSession sid
    , UpdateEvent $ \(DeleteSession sid)                -> deleteSession sid
    , UpdateEvent $ \(DeleteAllSessionsOfAuthId authId) -> deleteAllSessionsOfAuthId authId
    , UpdateEvent $ \(InsertSession session)            -> insertSession session
    , UpdateEvent $ \(ReplaceSession session)           -> replaceSession session ]
