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
  , throwSS
  ) where

import Control.Applicative as A
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid as M
import Data.Proxy (Proxy(..))
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Database.Persist (PersistEntity(..))
import Web.PathPieces (PathPiece)
import Web.ServerSession.Core

import qualified Control.Exception as E
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P

import Web.ServerSession.Backend.Persistent.Internal.Types


-- We can't use the Template Haskell since we want to generalize
-- some fields.
--
-- This is going to be a pain to upgrade when the next major
-- persistent version comes :(.

-- | Entity corresponding to a 'Session'.
--
-- We're bending @persistent@ in ways it wasn't expected to.  In
-- particular, this entity is parametrized over the session type.
data PersistentSession sess =
  PersistentSession
    { persistentSessionKey        :: !(SessionId sess)    -- ^ Session ID, primary key.
    , persistentSessionAuthId     :: !(Maybe ByteStringJ) -- ^ Value of "_ID" session key.
    , persistentSessionSession    :: !(Decomposed sess)   -- ^ Rest of the session data.
    , persistentSessionCreatedAt  :: !UTCTime             -- ^ When this session was created.
    , persistentSessionAccessedAt :: !UTCTime             -- ^ When this session was last accessed.
    } deriving (Typeable)

deriving instance Eq   (Decomposed sess) => Eq   (PersistentSession sess)
deriving instance Ord  (Decomposed sess) => Ord  (PersistentSession sess)
deriving instance Show (Decomposed sess) => Show (PersistentSession sess)


type PersistentSessionId sess = Key (PersistentSession sess)

instance forall sess. P.PersistFieldSql (Decomposed sess) => P.PersistEntity (PersistentSession sess) where
  type PersistEntityBackend (PersistentSession sess) = P.SqlBackend

  data Unique (PersistentSession sess)

  newtype Key (PersistentSession sess) =
    PersistentSessionKey' {unPersistentSessionKey :: SessionId sess}
    deriving ( Eq, Ord, Show, Read, PathPiece
             , P.PersistField, P.PersistFieldSql, A.ToJSON, A.FromJSON )

  data EntityField (PersistentSession sess) typ =
      typ ~ PersistentSessionId sess => PersistentSessionId
    | typ ~ SessionId sess           => PersistentSessionKey
    | typ ~ Maybe ByteStringJ        => PersistentSessionAuthId
    | typ ~ Decomposed sess          => PersistentSessionSession
    | typ ~ UTCTime                  => PersistentSessionCreatedAt
    | typ ~ UTCTime                  => PersistentSessionAccessedAt

  keyToValues = (:[]) . P.toPersistValue . unPersistentSessionKey
  keyFromValues [x] | Right v <- P.fromPersistValue x = Right $ PersistentSessionKey' v
  keyFromValues xs  = Left $ T.pack $ "PersistentSession/keyFromValues: " ++ show xs

  entityDef _
    = P.EntityDef
        (P.HaskellName "PersistentSession")
        (P.DBName "persistent_session")
        (pfd PersistentSessionId)
        ["json"]
        [ pfd PersistentSessionKey
        , pfd PersistentSessionAuthId
        , pfd PersistentSessionSession
        , pfd PersistentSessionCreatedAt
        , pfd PersistentSessionAccessedAt ]
        []
        []
        ["Eq", "Ord", "Show", "Typeable"]
        M.mempty
        False
    where
      pfd :: P.EntityField (PersistentSession sess) typ -> P.FieldDef
      pfd = P.persistFieldDef

  toPersistFields (PersistentSession a b c d e) =
    [ P.SomePersistField a
    , P.SomePersistField b
    , P.SomePersistField c
    , P.SomePersistField d
    , P.SomePersistField e ]

  fromPersistValues [a, b, c, d, e] =
    PersistentSession
      A.<$> err "key"        (P.fromPersistValue a)
      <*> err "authId"     (P.fromPersistValue b)
      <*> err "session"    (P.fromPersistValue c)
      <*> err "createdAt"  (P.fromPersistValue d)
      <*> err "accessedAt" (P.fromPersistValue e)
    where
      err :: T.Text -> Either T.Text a -> Either T.Text a
      err s (Left r)  = Left $ T.concat ["PersistentSession/fromPersistValues/", s, ": ", r]
      err _ (Right v) = Right v
  fromPersistValues x = Left $ T.pack $ "PersistentSession/fromPersistValues: " ++ show x

  persistUniqueToFieldNames _ = error "Degenerate case, should never happen"
  persistUniqueToValues _     = error "Degenerate case, should never happen"
  persistUniqueKeys _         = []

  persistFieldDef PersistentSessionId
    = P.FieldDef
        (P.HaskellName "Id")
        (P.DBName "id")
        (P.FTTypeCon
           Nothing "PersistentSessionId")
        (P.SqlOther "Composite Reference")
        []
        True
        (P.CompositeRef
           (P.CompositeDef
              [P.FieldDef
                 (P.HaskellName "key")
                 (P.DBName "key")
                 (P.FTTypeCon Nothing "SessionId")
                 (P.SqlOther "SqlType unset for key")
                 []
                 True
                 P.NoReference]
              []))
  persistFieldDef PersistentSessionKey
    = P.FieldDef
        (P.HaskellName "key")
        (P.DBName "key")
        (P.FTTypeCon Nothing "SessionId sess")
        (P.sqlType (Proxy :: Proxy (SessionId sess)))
        ["maxlen=30"]
        True
        P.NoReference
  persistFieldDef PersistentSessionAuthId
    = P.FieldDef
        (P.HaskellName "authId")
        (P.DBName "auth_id")
        (P.FTTypeCon Nothing "ByteStringJ")
        (P.sqlType (Proxy :: Proxy ByteStringJ))
        ["Maybe"]
        True
        P.NoReference
  persistFieldDef PersistentSessionSession
    = P.FieldDef
        (P.HaskellName "session")
        (P.DBName "session")
        (P.FTTypeCon Nothing "Decomposed sess")
        (P.sqlType (Proxy :: Proxy (Decomposed sess))) -- Important!
        []
        True
        P.NoReference
  persistFieldDef PersistentSessionCreatedAt
    = P.FieldDef
        (P.HaskellName "createdAt")
        (P.DBName "created_at")
        (P.FTTypeCon Nothing "UTCTime")
        (P.sqlType (Proxy :: Proxy UTCTime))
        []
        True
        P.NoReference
  persistFieldDef PersistentSessionAccessedAt
    = P.FieldDef
        (P.HaskellName "accessedAt")
        (P.DBName "accessed_at")
        (P.FTTypeCon Nothing "UTCTime")
        (P.sqlType (Proxy :: Proxy UTCTime))
        []
        True
        P.NoReference

  persistIdField = PersistentSessionId

  fieldLens PersistentSessionId = lensPTH
    P.entityKey
    (\(P.Entity _ v) k -> P.Entity k v)
  fieldLens PersistentSessionKey = lensPTH
    (persistentSessionKey . P.entityVal)
    (\(P.Entity k v) x -> P.Entity k (v {persistentSessionKey = x}))
  fieldLens PersistentSessionAuthId = lensPTH
    (persistentSessionAuthId . P.entityVal)
    (\(P.Entity k v) x -> P.Entity k (v {persistentSessionAuthId = x}))
  fieldLens PersistentSessionSession = lensPTH
    (persistentSessionSession . P.entityVal)
    (\(P.Entity k v) x -> P.Entity k (v {persistentSessionSession = x}))
  fieldLens PersistentSessionCreatedAt = lensPTH
    (persistentSessionCreatedAt . P.entityVal)
    (\(P.Entity k v) x -> P.Entity k (v {persistentSessionCreatedAt = x}))
  fieldLens PersistentSessionAccessedAt = lensPTH
    (persistentSessionAccessedAt . P.entityVal)
    (\(P.Entity k v) x -> P.Entity k (v {persistentSessionAccessedAt = x}))


-- | Copy-paste from @Database.Persist.TH@.  Who needs lens anyway...
lensPTH :: Functor f => (s -> a) -> (s -> b -> t) -> (a -> f b) -> s -> f t
lensPTH sa sbt afb s = fmap (sbt s) (afb $ sa s)


instance A.ToJSON (Decomposed sess) => A.ToJSON (PersistentSession sess) where
  toJSON (PersistentSession key authId session createdAt accessedAt) =
    A.object
      [ "key"        A..= key
      , "authId"     A..= authId
      , "session"    A..= session
      , "createdAt"  A..= createdAt
      , "accessedAt" A..= accessedAt ]

instance A.FromJSON (Decomposed sess) => A.FromJSON (PersistentSession sess) where
  parseJSON (A.Object obj) =
    PersistentSession
      <$> obj A..: "key"
      <*> obj A..: "authId"
      <*> obj A..: "session"
      <*> obj A..: "createdAt"
      <*> obj A..: "accessedAt"
  parseJSON _ = mempty

instance ( A.ToJSON (Decomposed sess)
         , P.PersistFieldSql (Decomposed sess)
         ) => A.ToJSON (P.Entity (PersistentSession sess)) where
  toJSON = P.entityIdToJSON

instance ( A.FromJSON (Decomposed sess)
         , P.PersistFieldSql (Decomposed sess)
         ) => A.FromJSON (P.Entity (PersistentSession sess)) where
  parseJSON = P.entityIdFromJSON


-- | Entity definitions needed to generate the SQL schema for
-- 'SqlStorage'.  Example using 'SessionMap':
--
-- @
-- serverSessionDefs (Proxy :: Proxy SessionMap)
-- @
serverSessionDefs :: forall sess. PersistEntity (PersistentSession sess) => Proxy sess -> [P.EntityDef]
serverSessionDefs _ = [entityDef (Proxy :: Proxy (PersistentSession sess))]


-- | Generate a key to the entity from the session ID.
psKey :: SessionId sess -> Key (PersistentSession sess)
psKey = PersistentSessionKey'


-- | Convert from 'Session' to 'PersistentSession'.
toPersistentSession :: Session sess -> PersistentSession sess
toPersistentSession Session {..} =
  PersistentSession
    { persistentSessionKey        = sessionKey
    , persistentSessionAuthId     = fmap B sessionAuthId
    , persistentSessionSession    = sessionData
    , persistentSessionCreatedAt  = sessionCreatedAt
    , persistentSessionAccessedAt = sessionAccessedAt
    }


-- | Convert from 'PersistentSession' to 'Session'.
fromPersistentSession :: PersistentSession sess -> Session sess
fromPersistentSession PersistentSession {..} =
  Session
    { sessionKey        = persistentSessionKey
    , sessionAuthId     = fmap unB persistentSessionAuthId
    , sessionData       = persistentSessionSession
    , sessionCreatedAt  = persistentSessionCreatedAt
    , sessionAccessedAt = persistentSessionAccessedAt
    }


-- | SQL session storage backend using @persistent@.
newtype SqlStorage sess =
  SqlStorage
    { connPool :: P.ConnectionPool
      -- ^ Pool of DB connections.  You may use the same pool as
      -- your application.
    } deriving (Typeable)


instance forall sess.
         ( IsSessionData sess
         , P.PersistFieldSql (Decomposed sess)
         ) => Storage (SqlStorage sess) where
  type SessionData  (SqlStorage sess) = sess
  type TransactionM (SqlStorage sess) = P.SqlPersistT IO
  runTransactionM  = flip P.runSqlPool . connPool
  getSession     _ = fmap (fmap fromPersistentSession) . P.get . psKey
  deleteSession  _ = P.delete . psKey
  deleteAllSessionsOfAuthId _ authId =
    P.deleteWhere [field P.==. Just (B authId)]
    where
      field :: EntityField (PersistentSession sess) (Maybe ByteStringJ)
      field = PersistentSessionAuthId
  insertSession s session = do
    mold <- getSession s (sessionKey session)
    maybe
      (void $ P.insert $ toPersistentSession session)
      (\old -> throwSS $ SessionAlreadyExists old session)
      mold
  replaceSession _ session = do
    let key = psKey $ sessionKey session
    mold <- P.get key
    maybe
      (throwSS $ SessionDoesNotExist session)
      (\_old -> void $ P.replace key $ toPersistentSession session)
      mold


-- | Specialization of 'E.throwIO' for 'SqlStorage'.
throwSS
  :: Storage (SqlStorage sess)
  => StorageException (SqlStorage sess)
  -> TransactionM (SqlStorage sess) a
throwSS = liftIO . E.throwIO
