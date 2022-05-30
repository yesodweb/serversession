-- | Storage backend for @serversession@ using persistent.
--
-- In order to use this backend, you have to include
-- 'serverSessionDefs' on your migration code.  For example,
-- the Yesod scaffold usually includes the following code:
--
-- @
-- -- On Model.hs
-- share [mkPersist sqlSettings, mkMigrate \"migrateAll\"]
--
-- -- On Application.hs
-- makeFoundation =
--     ...
--     runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc
--     ...
-- @
--
-- You should changed those lines to:
--
-- @
-- -- On Model.hs
-- share [mkPersist sqlSettings, mkSave \"entityDefs\"]
--
-- -- On Application.hs
-- import Web.ServerSession.Backend.Persistent
--
-- mkMigrate \"migrateAll\" (entityDefs \`embedEntityDefs\` serverSessionDefsBySessionMap)
--
-- makeFoundation =
--     ...
--     runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc
--     ...
-- @
--
-- If you're not using @SessionMap@, just use 'mkServerSessionDefs' and change @Proxy@ type above.
--
-- If you forget to setup the migration above, this session
-- storage backend will fail at runtime as the required table
-- will not exist.
module Web.ServerSession.Backend.Persistent
  ( SqlStorage(..)
  , PersistentSession(..)
  , PersistentSessionId
  , serverSessionDefsBySessionMap
  , PersistentSessionBySessionMap
  , mkServerSessionDefs
  ) where

import Web.ServerSession.Backend.Persistent.Internal.Impl
