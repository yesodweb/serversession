{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Internal module exposing the guts of the package.  Use at
-- your own risk.  No API stability guarantees apply.
--
-- Also exports orphan instances of @PersistField{,Sql} SessionId@.
module Web.ServerSession.Backend.Persistent.Internal.Types
  ( ByteStringJ(..)
    -- * Orphan instances
    -- ** SessionId
    -- $orphanSessionId
    -- ** SessionMap
    -- $orphanSessionMap
  ) where

import Control.Arrow (first)
import Control.Monad ((>=>), mzero)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.Persist (PersistField(..))
import Database.Persist.Sql (PersistFieldSql(..), SqlType(..))
import Web.ServerSession.Core
import Web.ServerSession.Core.Internal (SessionId(..))

import qualified Data.Aeson as A
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.Map as M
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE


----------------------------------------------------------------------


-- $orphanSessionId
--
-- @
-- instance 'PersistField'    ('SessionId' sess)
-- instance 'PersistFieldSql' ('SessionId' sess)
-- @
--
-- Does not do sanity checks (DB is trusted).
instance PersistField (SessionId sess) where
  toPersistValue = toPersistValue . unS
  fromPersistValue = fmap S . fromPersistValue

instance PersistFieldSql (SessionId sess) where
  sqlType p = sqlType (fmap unS p)


----------------------------------------------------------------------


-- | Newtype of a 'ByteString' with JSON support via base64url.
newtype ByteStringJ = B { unB :: ByteString }
  deriving (Eq, Ord, Show, Read, Typeable)

instance PersistField ByteStringJ where
  toPersistValue = toPersistValue . unB
  fromPersistValue = fmap B . fromPersistValue

instance PersistFieldSql ByteStringJ where
  sqlType p = sqlType (fmap unB p)

instance A.FromJSON ByteStringJ where
  parseJSON (A.String t) =
    either (const mzero) (return . B) $
    B64URL.decode $
    TE.encodeUtf8 t
  parseJSON _ = mzero

instance A.ToJSON ByteStringJ where
  toJSON = A.String . TE.decodeUtf8 . B64URL.encode . unB


----------------------------------------------------------------------


-- $orphanSessionMap
--
-- @
-- instance 'PersistField'    'SessionMap'
-- instance 'PersistFieldSql' 'SessionMap'
-- instance 'S.Serialize'       'SessionMap'
-- instance 'A.FromJSON'        'SessionMap'
-- instance 'A.ToJSON'          'SessionMap'
-- @

-- 'PersistField' for 'SessionMap' serializes using @cereal@ on
-- the database.  We tried to use @aeson@ but @cereal@ is twice
-- faster and uses half the memory for this use case.
--
-- The JSON instance translates to objects using base64url for
-- the values of 'ByteString' (cf. 'ByteStringJ').
instance PersistField SessionMap where
  toPersistValue   = toPersistValue . S.encode
  fromPersistValue = fromPersistValue >=> (either (Left . T.pack) Right . S.decode)

instance PersistFieldSql SessionMap where
  sqlType _ = SqlBlob

instance S.Serialize SessionMap where
  put = S.put . map (first TE.encodeUtf8) . M.toAscList . unSessionMap
  get = SessionMap . M.fromAscList . map (first TE.decodeUtf8) <$> S.get

instance A.FromJSON SessionMap where
  parseJSON = fmap fixup . A.parseJSON
    where
      fixup :: M.Map Text ByteStringJ -> SessionMap
      fixup = SessionMap . fmap unB

instance A.ToJSON SessionMap where
  toJSON = A.toJSON . mangle
    where
      mangle :: SessionMap -> M.Map Text ByteStringJ
      mangle = fmap B . unSessionMap
