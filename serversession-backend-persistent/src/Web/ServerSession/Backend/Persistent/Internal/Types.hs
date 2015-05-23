{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Internal module exposing the guts of the package.  Use at
-- your own risk.  No API stability guarantees apply.
--
-- Also exports orphan instances of @PersistField{,Sql} SessionId@.
module Web.ServerSession.Backend.Persistent.Internal.Types
  ( ByteStringJ(..)
  , SessionMapJ(..)
  ) where

import Control.Monad ((>=>), mzero)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.Persist (PersistField(..))
import Database.Persist.Sql (PersistFieldSql(..))
import Web.ServerSession.Core
import Web.ServerSession.Core.Internal (SessionId(..))

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE


----------------------------------------------------------------------


-- | Does not do sanity checks (DB is trusted).
instance PersistField SessionId where
  toPersistValue = toPersistValue . unS
  fromPersistValue = fmap S . fromPersistValue

instance PersistFieldSql SessionId where
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


-- | Newtype of a 'SessionMap' that serializes as a JSON on
-- the database.  We use JSON because it's easy to inspect for a
-- human.
newtype SessionMapJ = M { unM :: SessionMap }
  deriving (Eq, Ord, Show, Read, Typeable)

encodeT :: A.ToJSON a => a -> Text
encodeT = TE.decodeUtf8 . L.toStrict . A.encode

decodeT :: A.FromJSON a => Text -> Either Text a
decodeT = either (Left . T.pack) Right . A.eitherDecode . L.fromStrict . TE.encodeUtf8

instance PersistField SessionMapJ where
  toPersistValue = toPersistValue . encodeT
  fromPersistValue = fromPersistValue >=> decodeT

instance PersistFieldSql SessionMapJ where
  sqlType p = sqlType (fmap encodeT p)

instance A.FromJSON SessionMapJ where
  parseJSON = fmap fixup . A.parseJSON
    where
      fixup :: M.Map Text ByteStringJ -> SessionMapJ
      fixup = M . fmap unB

instance A.ToJSON SessionMapJ where
  toJSON = A.toJSON . mangle
    where
      mangle :: SessionMapJ -> M.Map Text ByteStringJ
      mangle = fmap B . unM
