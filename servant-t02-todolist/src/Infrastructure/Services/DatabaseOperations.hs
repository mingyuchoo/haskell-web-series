module Infrastructure.Services.DatabaseOperations
    ( initializeDatabase
    ) where

-- Import the concrete implementation (but keep it hidden from upper layers)
import Infrastructure.Repositories.SQLiteTodoRepository (migrate)

-- -------------------------------------------------------------------
-- Use cases for database operations
-- -------------------------------------------------------------------

-- Initialize the database (create tables, etc.)
initializeDatabase :: IO ()
initializeDatabase = migrate
