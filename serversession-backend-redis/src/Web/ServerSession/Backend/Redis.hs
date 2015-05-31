-- | Storage backend for @serversession@ using Redis via @hedis@.
module Web.ServerSession.Backend.Redis
  ( RedisStorage(..)
  , RedisStorageException(..)
  , RedisSession(..)
  ) where

import Web.ServerSession.Backend.Redis.Internal
