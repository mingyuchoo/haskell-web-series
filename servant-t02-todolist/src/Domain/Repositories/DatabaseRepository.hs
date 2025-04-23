module Domain.Repositories.DatabaseRepository
    ( DatabaseRepository(..) 
    ) where

-- -------------------------------------------------------------------
-- Repository
-- -------------------------------------------------------------------
    
-- Repository interface for database operations
class DatabaseRepository m where
    migrateDatabase :: m ()
