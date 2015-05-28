-- | Internal module exposing the guts of the package.  Use at
-- your own risk.  No API stability guarantees apply.
module Web.ServerSession.Backend.Persistent.Internal.Impl
  ( PersistentSession(..)
  , PersistentSessionId
  , EntityField(..)
  , serverSessionDefs
  , psKey
  , toPersistentSession
  , fromPersistentSession
  , SqlStorage(..)
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Database.Persist (PersistEntity(..))
import Database.Persist.TH (mkPersist, mkSave, persistLowerCase, share, sqlSettings)
import Web.ServerSession.Core

import qualified Control.Exception as E
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P

import Web.ServerSession.Backend.Persistent.Internal.Types


share
  [mkPersist sqlSettings, mkSave "serverSessionDefs"]
  [persistLowerCase|
    PersistentSession json
      key        SessionId         -- Session ID, primary key.
      authId     ByteStringJ Maybe -- Value of "_ID" session key.
      session    SessionMapJ       -- Rest of the session data.
      createdAt  UTCTime           -- When this session was created.
      accessedAt UTCTime           -- When this session was last accessed.
      Primary key
      deriving Eq Ord Show Typeable
  |]


-- | Generate a key to the entity from the session ID.
psKey :: SessionId -> Key PersistentSession
psKey = PersistentSessionKey'


-- | Convert from 'Session' to 'PersistentSession'.
toPersistentSession :: Session -> PersistentSession
toPersistentSession Session {..} =
  PersistentSession
    { persistentSessionKey        = sessionKey
    , persistentSessionAuthId     = fmap B sessionAuthId
    , persistentSessionSession    = M sessionData
    , persistentSessionCreatedAt  = sessionCreatedAt
    , persistentSessionAccessedAt = sessionAccessedAt
    }


-- | Convert from 'PersistentSession' to 'Session'.
fromPersistentSession :: PersistentSession -> Session
fromPersistentSession PersistentSession {..} =
  Session
    { sessionKey        = persistentSessionKey
    , sessionAuthId     = fmap unB persistentSessionAuthId
    , sessionData       = unM persistentSessionSession
    , sessionCreatedAt  = persistentSessionCreatedAt
    , sessionAccessedAt = persistentSessionAccessedAt
    }


-- | SQL session storage backend using @persistent@.
newtype SqlStorage =
  SqlStorage
    { connPool :: P.ConnectionPool
      -- ^ Pool of DB connections.  You may use the same pool as
      -- your application.
    } deriving (Typeable)


instance Storage SqlStorage where
  type TransactionM SqlStorage = P.SqlPersistT IO
  runTransactionM  = flip P.runSqlPool . connPool
  getSession     _ = fmap (fmap fromPersistentSession) . P.get . psKey
  deleteSession  _ = P.delete . psKey
  deleteAllSessionsOfAuthId _ authId = P.deleteWhere [PersistentSessionAuthId P.==. Just (B authId)]
  insertSession s session = do
    mold <- getSession s (sessionKey session)
    maybe
      (void $ P.insert $ toPersistentSession session)
      (\old -> liftIO $ E.throwIO $ SessionAlreadyExists old session)
      mold
  replaceSession _ session = do
    let key = psKey $ sessionKey session
    mold <- P.get key
    maybe
      (liftIO $ E.throwIO $ SessionDoesNotExist session)
      (\_old -> void $ P.replace key $ toPersistentSession session)
      mold
