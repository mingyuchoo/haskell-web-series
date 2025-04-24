module Infrastructure.Repositories.DatabaseRepository
    ( migrateDatabase,
      initializeDatabase
    ) where

import Infrastructure.Repositories.Operations.DatabaseOperations
    ( initializeDatabase
    )

-- -------------------------------------------------------------------
-- Repository
-- -------------------------------------------------------------------
    
-- Repository interface for database operations
class DatabaseRepository m where
    migrateDatabase :: m ()
