{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Domain.Repositories.Entities.Todo
    ( Todo(..) 
    , NewTodo(..)
    , ValidationError(..)
    , Priority(..)
    , validateTodoTitle
    ) where

import           Data.Aeson                     (FromJSON, ToJSON)
import           Data.Text                      (Text, pack)
import           Data.Time                      (UTCTime)
import           Database.SQLite.Simple         (FromRow (..), ToRow (..), field)
import           GHC.Generics                   (Generic)

-- -------------------------------------------------------------------
-- Entities
-- -------------------------------------------------------------------

-- Priority levels for todos
data Priority = Low | Medium | High
    deriving (Eq, Generic, Read)
    
-- Show instance for Priority
instance Show Priority where
    show Low = "Low"
    show Medium = "Medium"
    show High = "High"

-- JSON instances for Priority
instance FromJSON Priority
instance ToJSON Priority

-- Core entity
data Todo = Todo 
    { todoId       :: Int
    , todoTitle    :: String
    , createdAt    :: UTCTime
    , priority     :: Priority
    , isCompleted  :: Bool
    }
    deriving (Eq, Generic, Show)

-- Used for creating a new todo without specifying todoId
newtype NewTodo = NewTodo
    { newTodoName     :: String
    }
    deriving (Eq, Generic, Show)



-- Validation error response
newtype ValidationError = ValidationError 
    { errorMessage :: Text 
    }
    deriving (Eq, Generic, Show)

-- JSON instances
instance FromJSON Todo
instance ToJSON Todo

instance FromJSON NewTodo
instance ToJSON NewTodo

instance ToJSON ValidationError

-- Database mapping instances
instance FromRow Todo where
  fromRow = do
    tId <- field
    tTitle <- field
    tCreatedAt <- field
    tPriorityStr <- field
    tIsCompleted <- field
    let tPriority = case tPriorityStr of
          "Low" -> Low
          "Medium" -> Medium
          "High" -> High
          _ -> Medium  -- Default to Medium if unknown
    return $ Todo tId tTitle tCreatedAt tPriority tIsCompleted

instance ToRow Todo where
  toRow (Todo tId tTitle tCreatedAt tPriority tIsCompleted) = toRow (tId, tTitle, tCreatedAt, show tPriority, tIsCompleted)

-- Validation functions
validateTodoTitle :: String -> Either ValidationError ()
validateTodoTitle title
  | null title = Left $ ValidationError (pack "TodoTitle cannot be empty")
  | length title < 3 = Left $ ValidationError (pack "TodoTitle must be at least 3 characters long") 
  | length title > 50 = Left $ ValidationError (pack "TodoTitle must be at most 50 characters long")
  | otherwise = Right ()
