-- | Internal module exposing the guts of the package.  Use at
-- your own risk.  No API stability guarantees apply.
module Web.ServerSession.Backend.Acid.Internal
  ( SessionIdToSession
  , AuthIdToSessionId
  , ServerSessionAcidState(..)
  , emptyState
  , removeSessionFromAuthId
  , nothingfy

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

import Control.Monad (when)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, modify')
import Data.Acid (AcidState, Query, Update, makeAcidic, query, update)
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Typeable (Typeable)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Web.ServerSession.Core as SS


----------------------------------------------------------------------


-- | Map from session IDs to sessions.  The most important map,
-- allowing us efficient access to a session given its ID.
type SessionIdToSession = M.Map SS.SessionId SS.Session


-- | Map from auth IDs to session IDs.  Allow us to invalidate
-- all sessions of given user without having to iterate through
-- the whole 'SessionIdToSession' map.
type AuthIdToSessionId  = M.Map SS.AuthId (S.Set SS.SessionId)


-- | The current sessions.
--
-- Besides the obvious map from session IDs to sessions, we also
-- maintain a map of auth IDs to session IDs.  This allow us to
-- quickly invalidate all sessions of a given user.
data ServerSessionAcidState =
  ServerSessionAcidState
    { sessionIdToSession :: !SessionIdToSession
    , authIdToSessionId  :: !AuthIdToSessionId
    } deriving (Show, Typeable)

deriveSafeCopy 0 'base ''SS.SessionId -- dangerous!
deriveSafeCopy 0 'base ''SS.Session   -- dangerous!
deriveSafeCopy 0 'base ''ServerSessionAcidState


-- | Empty 'ServerSessionAcidState' used to bootstrap the 'AcidState'.
emptyState :: ServerSessionAcidState
emptyState = ServerSessionAcidState M.empty M.empty


-- | Remove the given 'SessionId' from the set of the given
-- 'AuthId' on the map.  Does not do anything if no 'AuthId' is
-- provided.
removeSessionFromAuthId :: SS.SessionId -> Maybe SS.AuthId -> AuthIdToSessionId -> AuthIdToSessionId
removeSessionFromAuthId sid = maybe id (M.update (nothingfy . S.delete sid))


-- | Change a 'S.Set' to 'Nothing' if it's 'S.null'.
nothingfy :: S.Set a -> Maybe (S.Set a)
nothingfy s = if S.null s then Nothing else Just s


----------------------------------------------------------------------


-- | Get the session for the given session ID.
getSession :: SS.SessionId -> Query ServerSessionAcidState (Maybe SS.Session)
getSession sid = M.lookup sid . sessionIdToSession <$> ask


-- | Delete the session with given session ID.
deleteSession :: SS.SessionId -> Update ServerSessionAcidState ()
deleteSession sid = do
  let removeSession = M.updateLookupWithKey (\_ _ -> Nothing) sid
  modify' $ \state ->
    let (oldSession, newSessionIdToSession) = removeSession $ sessionIdToSession state
        newAuthIdToSessionId = removeSessionFromAuthId sid mauthId $ authIdToSessionId state
          where mauthId = oldSession >>= SS.sessionAuthId
    in ServerSessionAcidState newSessionIdToSession newAuthIdToSessionId


-- | Delete all sessions of the given auth ID.
deleteAllSessionsOfAuthId :: SS.AuthId -> Update ServerSessionAcidState ()
deleteAllSessionsOfAuthId authId = do
  let removeSession = maybe id (flip M.difference . M.fromSet (const ()))
      removeAuth    = M.updateLookupWithKey (\_ _ -> Nothing) authId
  modify' $ \state ->
    let (sessionIds, newAuthIdToSessionId) = removeAuth $ authIdToSessionId state
        newSessionIdToSession = removeSession sessionIds $ sessionIdToSession state
    in ServerSessionAcidState newSessionIdToSession newAuthIdToSessionId


-- | Insert a new session.
insertSession :: SS.Session -> Update ServerSessionAcidState ()
insertSession session = do
  let insertSess = M.insert sid session
      insertAuth = maybe id (flip (M.insertWith S.union) (S.singleton sid)) (SS.sessionAuthId session)
      sid        = SS.sessionKey session
  modify' $ \state ->
    ServerSessionAcidState
      (insertSess $ sessionIdToSession state)
      (insertAuth $ authIdToSessionId  state)


-- | Replace the contents of a session.
replaceSession :: SS.Session -> Update ServerSessionAcidState ()
replaceSession session = do
  -- Remove the old auth ID from the map if it has changed.
  let sid = SS.sessionKey session
  oldSession <- M.lookup sid . sessionIdToSession <$> get
  let oldAuthId = SS.sessionAuthId =<< oldSession
  when (oldAuthId /= SS.sessionAuthId session) $
    modify' $ \state -> state
      { authIdToSessionId = removeSessionFromAuthId sid oldAuthId $ authIdToSessionId state
      }
  -- Otherwise the operation is the same as inserting.
  insertSession session


----------------------------------------------------------------------


makeAcidic ''ServerSessionAcidState ['getSession, 'deleteSession, 'deleteAllSessionsOfAuthId, 'insertSession, 'replaceSession]


-- | Session storage backend using @acid-state@.
newtype AcidStorage =
  AcidStorage
    { acidState :: AcidState ServerSessionAcidState
      -- ^ Open 'AcidState' of server sessions.
    } deriving (Typeable)


-- | We do not provide any ACID guarantees for different actions
-- running inside the same @TransactionM AcidStorage@.
instance SS.Storage AcidStorage where
  type TransactionM AcidStorage = IO
  runTransactionM = const id
  getSession                (AcidStorage s) = query  s . GetSession
  deleteSession             (AcidStorage s) = update s . DeleteSession
  deleteAllSessionsOfAuthId (AcidStorage s) = update s . DeleteAllSessionsOfAuthId
  insertSession             (AcidStorage s) = update s . InsertSession
  replaceSession            (AcidStorage s) = update s . ReplaceSession
