module Application.UseCases.DatabaseOperations
    ( initializeDatabase
    ) where

-- Import the repository interface
import Domain.Repositories.DatabaseRepository (DatabaseRepository(..))

-- Import the concrete implementation (but keep it hidden from upper layers)
import Infrastructure.Persistence.SQLiteTodoRepository ()

-- -------------------------------------------------------------------
-- Use cases for database operations
-- -------------------------------------------------------------------

-- Initialize the database (create tables, etc.)
initializeDatabase :: IO ()
initializeDatabase = migrateDatabase
