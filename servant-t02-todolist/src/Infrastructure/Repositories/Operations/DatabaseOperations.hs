-- | Database initialization operations
-- 
-- This module provides functions for initializing and managing the database.
-- It acts as a facade to hide the concrete implementation details from upper layers.
module Infrastructure.Repositories.Operations.DatabaseOperations
    ( -- * Database Operations
      initializeDatabase
    ) where

-- Import the concrete implementation (but keep it hidden from upper layers)
import Infrastructure.Repositories.SQLiteTodoRepository (migrate)

-- -------------------------------------------------------------------
-- Database Operations
-- -------------------------------------------------------------------

-- | Initialize the database
-- 
-- Creates the necessary tables if they don't exist.
-- This function should be called when the application starts.
initializeDatabase :: IO ()
initializeDatabase = migrate
