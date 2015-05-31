-- | Internal module exposing the guts of the package.  Use at
-- your own risk.  No API stability guarantees apply.
module Web.ServerSession.Backend.Redis.Internal
  ( RedisStorage(..)
  , RedisStorageException(..)

  , transaction
  , unwrap
  , rSessionKey
  , rAuthKey

  , RedisSession(..)
  , parseSession
  , printSession
  , parseUTCTime
  , printUTCTime
  , timeFormat

  , getSessionImpl
  , deleteSessionImpl
  , removeSessionFromAuthId
  , insertSessionForAuthId
  , deleteAllSessionsOfAuthIdImpl
  , insertSessionImpl
  , replaceSessionImpl
  , throwRS
  ) where

import Control.Applicative ((<$))
import Control.Arrow (first)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
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
newtype RedisStorage sess =
  RedisStorage
    { connPool :: R.Connection
      -- ^ Connection pool to the Redis server.
    } deriving (Typeable)


-- | We do not provide any ACID guarantees for different actions
-- running inside the same @TransactionM RedisStorage@.
instance RedisSession sess => Storage (RedisStorage sess) where
  type SessionData  (RedisStorage sess) = sess
  type TransactionM (RedisStorage sess) = R.Redis
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
rSessionKey :: SessionId sess -> ByteString
rSessionKey = B.append "ssr:session:" . TE.encodeUtf8 . toPathPiece


-- | Redis key for the given auth ID.
rAuthKey :: AuthId -> ByteString
rAuthKey = B.append "ssr:authid:"


----------------------------------------------------------------------


-- | Class for data types that can be used as session data for
-- the Redis backend.
--
-- It should hold that
--
-- @
-- fromHash p . perm . toHash p  ===  id
-- @
--
-- for all list permutations @perm :: [a] -> [a]@,
-- where @p :: Proxy sess@.
class IsSessionData sess => RedisSession sess where
  -- | Transform a decomposed session into a Redis hash.  Keys
  -- will be prepended with @\"data:\"@ before being stored.
  toHash   :: Proxy sess -> Decomposed sess -> [(ByteString, ByteString)]

  -- | Parse back a Redis hash into session data.
  fromHash :: Proxy sess -> [(ByteString, ByteString)] -> Decomposed sess


-- | Assumes that keys are UTF-8 encoded when parsing (which is
-- true if keys are always generated via @toHash@).
instance RedisSession SessionMap where
  toHash   _ = map (first TE.encodeUtf8) . M.toList . unSessionMap
  fromHash _ = SessionMap . M.fromList . map (first TE.decodeUtf8)


-- | Parse a 'Session' from a Redis hash.
parseSession
  :: forall sess. RedisSession sess
  => SessionId sess
  -> [(ByteString, ByteString)]
  -> Maybe (Session sess)
parseSession _   []  = Nothing
parseSession sid bss =
  let (externalList, internalList) = partition (B8.isPrefixOf "data:" . fst) bss
      authId     = lookup "internal:authId" internalList
      createdAt  = parseUTCTime $ lookup' "internal:createdAt"
      accessedAt = parseUTCTime $ lookup' "internal:accessedAt"
      lookup' k = fromMaybe (error err) $ lookup k internalList
        where err = "serversession-backend-redis/parseSession: missing key " ++ show k
      data_ = fromHash p $ map (first removePrefix) externalList
        where removePrefix bs = let ("data:", key) = B8.splitAt 5 bs in key
              p = Proxy :: Proxy sess
  in Just Session
       { sessionKey        = sid
       , sessionAuthId     = authId
       , sessionData       = data_
       , sessionCreatedAt  = createdAt
       , sessionAccessedAt = accessedAt
       }


-- | Convert a 'Session' into a Redis hash.
printSession :: forall sess. RedisSession sess => Session sess -> [(ByteString, ByteString)]
printSession Session {..} =
  maybe id ((:) . (,) "internal:authId") sessionAuthId $
  (:) ("internal:createdAt",  printUTCTime sessionCreatedAt) $
  (:) ("internal:accessedAt", printUTCTime sessionAccessedAt) $
  map (first $ B8.append "data:") $
  toHash (Proxy :: Proxy sess) sessionData


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
timeFormat = "%Y-%m-%dT%H:%M:%S%Q"


----------------------------------------------------------------------


-- | Run the given Redis command in batches of @511*1024@ items.
-- This is used for @HMSET@ because there's a hard Redis limit of
-- @1024*1024@ arguments to a command.  The last result is returned.
batched :: Monad m => ([a] -> m b) -> [a] -> m b
batched f xs =
  let (this, rest) = splitAt (511*1024) xs
      continue | null rest = return
               | otherwise = const (batched f rest)
  in f this >>= continue


-- | Get the session for the given session ID.
getSessionImpl :: RedisSession sess => SessionId sess -> R.Redis (Maybe (Session sess))
getSessionImpl sid = parseSession sid <$> unwrap (R.hgetall $ rSessionKey sid)


-- | Delete the session with given session ID.
deleteSessionImpl :: RedisSession sess => SessionId sess -> R.Redis ()
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
removeSessionFromAuthId :: R.RedisCtx m f => SessionId sess -> Maybe AuthId -> m ()
removeSessionFromAuthId = fooSessionBarAuthId R.srem

-- | Insert the given 'SessionId' into the set of sessions of the
-- given 'AuthId'.  Does not do anything if @Nothing@.
insertSessionForAuthId :: R.RedisCtx m f => SessionId sess -> Maybe AuthId -> m ()
insertSessionForAuthId = fooSessionBarAuthId R.sadd


-- | (Internal) Helper for 'removeSessionFromAuthId' and 'insertSessionForAuthId'
fooSessionBarAuthId
  :: R.RedisCtx m f
  => (ByteString -> [ByteString] -> m (f Integer))
  -> SessionId sess
  -> Maybe AuthId
  -> m ()
fooSessionBarAuthId _   _   Nothing       = return ()
fooSessionBarAuthId fun sid (Just authId) = void $ fun (rAuthKey authId) [rSessionKey sid]


-- | Delete all sessions of the given auth ID.
deleteAllSessionsOfAuthIdImpl :: AuthId -> R.Redis ()
deleteAllSessionsOfAuthIdImpl authId = do
  sessionRefs <- unwrap $ R.smembers (rAuthKey authId)
  void $ unwrap $ R.del $ rAuthKey authId : sessionRefs


-- | Insert a new session.
insertSessionImpl :: RedisSession sess => Session sess -> R.Redis ()
insertSessionImpl session = do
  -- Check that no old session exists.
  let sid = sessionKey session
  moldSession <- getSessionImpl sid
  case moldSession of
    Just oldSession -> throwRS $ SessionAlreadyExists oldSession session
    Nothing -> do
      transaction $ do
        let sk = rSessionKey sid
        r <- batched (R.hmset sk) (printSession session)
        -- TODO: R.expireat
        insertSessionForAuthId (sessionKey session) (sessionAuthId session)
        return (() <$ r)


-- | Replace the contents of a session.
replaceSessionImpl :: RedisSession sess => Session sess -> R.Redis ()
replaceSessionImpl session = do
  -- Check that the old session exists.
  let sid = sessionKey session
  moldSession <- getSessionImpl sid
  case moldSession of
    Nothing -> throwRS $ SessionDoesNotExist session
    Just oldSession -> do
      transaction $ do
        -- Delete the old session and set the new one.
        let sk = rSessionKey sid
        _ <- R.del [sk]
        r <- batched (R.hmset sk) (printSession session)

        -- Remove the old auth ID from the map if it has changed.
        let oldAuthId = sessionAuthId oldSession
            newAuthId = sessionAuthId session
        when (oldAuthId /= newAuthId) $ do
          removeSessionFromAuthId sid oldAuthId
          insertSessionForAuthId sid newAuthId

        return (() <$ r)


-- | Specialization of 'E.throwIO' for 'RedisStorage'.
throwRS
  :: Storage (RedisStorage sess)
  => StorageException (RedisStorage sess)
  -> R.Redis a
throwRS = liftIO . E.throwIO
