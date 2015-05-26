-- | Internal module exposing the guts of the package.  Use at
-- your own risk.  No API stability guarantees apply.
module Web.ServerSession.Backend.Redis.Internal
  ( RedisStorage(..)
  , RedisStorageException(..)

  , transaction
  , unwrap
  , rSessionKey
  , rAuthKey

  , parseSession
  , printSession
  , parseUTCTime
  , printUTCTime
  , timeFormat

  , getSessionImpl
  , deleteSessionImpl
  , removeSessionFromAuthId
  , deleteAllSessionsOfAuthIdImpl
  , insertSessionImpl
  , replaceSessionImpl
  ) where

import Control.Applicative ((<$))
import Control.Arrow (first)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Web.PathPieces (toPathPiece)
import Web.ServerSession.Core

import qualified Control.Exception as E
import qualified Database.Redis as R
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M
import qualified Data.Text.Encoding as TE
import qualified Data.Time.Clock as TI
import qualified Data.Time.Format as TI


----------------------------------------------------------------------


-- | Session storage backend using Redis via the @hedis@ package.
newtype RedisStorage =
  RedisStorage
    { connPool :: R.Connection
      -- ^ Connection pool to the Redis server.
    } deriving (Typeable)


-- | We do not provide any ACID guarantees for different actions
-- running inside the same @TransactionM RedisStorage@.
instance Storage RedisStorage where
  type TransactionM RedisStorage = R.Redis
  runTransactionM = R.runRedis . connPool
  getSession                _ = getSessionImpl
  deleteSession             _ = deleteSessionImpl
  deleteAllSessionsOfAuthId _ = deleteAllSessionsOfAuthIdImpl
  insertSession             _ = insertSessionImpl
  replaceSession            _ = replaceSessionImpl


-- | An exception thrown by the @serversession-backend-redis@
-- package.
data RedisStorageException =
    ExpectedTxSuccess (R.TxResult ())
    -- ^ We expected 'TxSuccess' but got something else.
  | ExpectedRight R.Reply
    -- ^ We expected 'Right' from an @Either 'R.Reply' a@ but got
    -- 'Left'.
    deriving (Show, Typeable)

instance E.Exception RedisStorageException


----------------------------------------------------------------------


-- | Run the given Redis transaction and force its result.
-- Throws a 'RedisStorageException' if the result is not
-- 'TxSuccess'.
transaction :: R.RedisTx (R.Queued ()) -> R.Redis ()
transaction tx = do
  ret <- R.multiExec tx
  case ret of
   R.TxSuccess () -> return ()
   _              -> liftIO $ E.throwIO $ ExpectedTxSuccess ret


-- | Unwraps an @Either 'R.Reply' a@ by throwing an exception if
-- not @Right@.
unwrap :: R.Redis (Either R.Reply a) -> R.Redis a
unwrap act = act >>= either (liftIO . E.throwIO . ExpectedRight) return


-- | Redis key for the given session ID.
rSessionKey :: SessionId -> ByteString
rSessionKey = B.append "ssr:session:" . TE.encodeUtf8 . toPathPiece


-- | Redis key for the given auth ID.
rAuthKey :: AuthId -> ByteString
rAuthKey = B.append "ssr:authid:"


----------------------------------------------------------------------


-- | Parse a 'Session' from a Redis hash.
parseSession :: SessionId -> [(ByteString, ByteString)] -> Maybe Session
parseSession _   []  = Nothing
parseSession sid bss =
  let (externalList, internalList) = partition (B8.isPrefixOf "data:" . fst) bss
      authId     = lookup "internal:authId" internalList
      createdAt  = parseUTCTime $ lookup' "internal:createdAt"
      accessedAt = parseUTCTime $ lookup' "internal:accessedAt"
      lookup' k = fromMaybe (error err) $ lookup k internalList
        where err = "serversession-backend-redis/parseSession: missing key " ++ show k
      sessionMap = M.fromList $ map (first $ TE.decodeUtf8 . removePrefix) externalList
        where removePrefix bs = let ("data:", key) = B8.splitAt 5 bs in key
  in Just Session
       { sessionKey        = sid
       , sessionAuthId     = authId
       , sessionData       = sessionMap
       , sessionCreatedAt  = createdAt
       , sessionAccessedAt = accessedAt
       }


-- | Convert a 'Session' into a Redis hash.
printSession :: Session -> [(ByteString, ByteString)]
printSession Session {..} =
  maybe id ((:) . (,) "internal:authId") sessionAuthId $
  (:) ("internal:createdAt",  printUTCTime sessionCreatedAt) $
  (:) ("internal:accessedAt", printUTCTime sessionAccessedAt) $
  map (first $ B8.append "data:" . TE.encodeUtf8) $
  M.toList sessionData


-- | Parse 'UTCTime' from a 'ByteString' stored on Redis.  Uses
-- 'error' on parse error.
parseUTCTime :: ByteString -> TI.UTCTime
parseUTCTime = TI.parseTimeOrError True TI.defaultTimeLocale timeFormat . B8.unpack


-- | Convert a 'UTCTime' into a 'ByteString' to be stored on
-- Redis.
printUTCTime :: TI.UTCTime -> ByteString
printUTCTime = B8.pack . TI.formatTime TI.defaultTimeLocale timeFormat


-- | Time format used when storing 'UTCTime'.
timeFormat :: String
timeFormat = "%Y-%m-%dT%H:%M:%S"


----------------------------------------------------------------------


-- | Get the session for the given session ID.
getSessionImpl :: SessionId -> R.Redis (Maybe Session)
getSessionImpl sid = parseSession sid <$> unwrap (R.hgetall $ rSessionKey sid)


-- | Delete the session with given session ID.
deleteSessionImpl :: SessionId -> R.Redis ()
deleteSessionImpl sid = do
  msession <- getSessionImpl sid
  case msession of
    Nothing -> return ()
    Just session ->
      transaction $ do
        r <- R.del [rSessionKey sid]
        removeSessionFromAuthId sid (sessionAuthId session)
        return (() <$ r)


-- | Remove the given 'SessionId' from the set of sessions of the
-- given 'AuthId'.  Does not do anything if @Nothing@.
removeSessionFromAuthId :: R.RedisCtx m f => SessionId -> Maybe AuthId -> m ()
removeSessionFromAuthId _   Nothing       = return ()
removeSessionFromAuthId sid (Just authId) =
  void $ R.srem (rAuthKey authId) [rSessionKey sid]


-- | Delete all sessions of the given auth ID.
deleteAllSessionsOfAuthIdImpl :: AuthId -> R.Redis ()
deleteAllSessionsOfAuthIdImpl authId = do
  sessionRefs <- unwrap $ R.smembers (rAuthKey authId)
  void $ unwrap $ R.del $ rAuthKey authId : sessionRefs


-- | Insert a new session.
insertSessionImpl :: Session -> R.Redis ()
insertSessionImpl session = do
  transaction $ do
    let sk = rSessionKey $ sessionKey session
    r <- R.hmset sk (printSession session)
    -- TODO: R.expireat
    maybe (return ()) (\authId -> void $ R.sadd (rAuthKey authId) [sk]) $ sessionAuthId session
    return (() <$ r)


-- | Replace the contents of a session.
replaceSessionImpl :: Session -> R.Redis ()
replaceSessionImpl session = do
  -- Remove the old auth ID from the map if it has changed.
  oldSession <- getSessionImpl (sessionKey session)
  let oldAuthId = sessionAuthId =<< oldSession
  when (oldAuthId /= sessionAuthId session) $
    removeSessionFromAuthId (sessionKey session) oldAuthId
  -- Otherwise the operation is the same as inserting.
  insertSessionImpl session
