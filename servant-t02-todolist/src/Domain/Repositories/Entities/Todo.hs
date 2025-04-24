{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Domain.Repositories.Entities.Todo
    ( Todo(..) 
    , NewTodo(..)
    , ValidationError(..)
    , Priority(..)
    , Status(..)
    , validateTodoTitle
    ) where

import           Data.Aeson                     (FromJSON(..), ToJSON(..), Value(..))
import           Data.Text                      (Text, pack, unpack)

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

-- Status levels for todos
data Status = TodoStatus | DoingStatus | DoneStatus
    deriving (Eq, Generic, Read)
    
-- Show instance for Status
instance Show Status where
    show TodoStatus = "Todo"
    show DoingStatus = "Doing"
    show DoneStatus = "Done"

-- Custom JSON instances for Status
instance FromJSON Status where
    parseJSON (String t) = case unpack t of
        "Todo" -> return TodoStatus
        "Doing" -> return DoingStatus
        "Done" -> return DoneStatus
        _ -> fail $ "Unknown status: " ++ unpack t
    parseJSON _ = fail "Expected String for Status"

instance ToJSON Status where
    toJSON TodoStatus = String (pack "Todo")
    toJSON DoingStatus = String (pack "Doing")
    toJSON DoneStatus = String (pack "Done")

-- Core entity
data Todo = Todo 
    { todoId       :: Int
    , todoTitle    :: String
    , createdAt    :: UTCTime
    , priority     :: Priority
    , status       :: Status
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
    tStatusStr <- field
    let tPriority = case tPriorityStr of
          "Low" -> Low
          "Medium" -> Medium
          "High" -> High
          _ -> Medium  -- Default to Medium if unknown
    let tStatus = case tStatusStr of
          "Done" -> DoneStatus
          "Doing" -> DoingStatus
          _ -> TodoStatus  -- Default to Todo if unknown
    return $ Todo tId tTitle tCreatedAt tPriority tStatus

instance ToRow Todo where
  toRow (Todo tId tTitle tCreatedAt tPriority tStatus) = toRow (tId, tTitle, tCreatedAt, show tPriority, show tStatus)

-- Validation functions
validateTodoTitle :: String -> Either ValidationError ()
validateTodoTitle title
  | null title = Left $ ValidationError (pack "TodoTitle cannot be empty")
  | length title < 3 = Left $ ValidationError (pack "TodoTitle must be at least 3 characters long") 
  | length title > 50 = Left $ ValidationError (pack "TodoTitle must be at most 50 characters long")
  | otherwise = Right ()
