{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Domain.Repositories.Entities.Todo
    ( Todo(..) 
    , NewTodo(..)
    , ValidationError(..)
    , validateTodoTitle
    ) where

import           Data.Aeson                     (FromJSON, ToJSON)
import           Data.Text                      (Text, pack)
import           Database.SQLite.Simple         (FromRow (..), ToRow (..), field)
import           GHC.Generics                   (Generic)

-- -------------------------------------------------------------------
-- Entities
-- -------------------------------------------------------------------

-- Core entity
data Todo = Todo 
    { todoId    :: Int
    , todoTitle :: String
    }
    deriving (Eq, Generic, Show)

-- Used for creating a new todo without specifying todoId
newtype NewTodo = NewTodo 
    { newTodoName :: String 
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
  fromRow = Todo <$> field <*> field

instance ToRow Todo where
  toRow (Todo tId tTitle) = toRow (tId, tTitle)

-- Validation functions
validateTodoTitle :: String -> Either ValidationError ()
validateTodoTitle title
  | null title = Left $ ValidationError (pack "TodoTitle cannot be empty")
  | length title < 3 = Left $ ValidationError (pack "TodoTitle must be at least 3 characters long")
  | length title > 50 = Left $ ValidationError (pack "TodoTitle must be at most 50 characters long")
  | otherwise = Right ()
