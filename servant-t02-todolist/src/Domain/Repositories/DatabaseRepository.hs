module Domain.Repositories.DatabaseRepository
    ( migrateDatabase
    ) where

-- -------------------------------------------------------------------
-- Repository
-- -------------------------------------------------------------------
    
-- Repository interface for database operations
class DatabaseRepository m where
    migrateDatabase :: m ()
