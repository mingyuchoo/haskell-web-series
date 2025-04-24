{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Infrastructure.Repositories.SQLiteTodoRepository
    ( SQLiteRepo(..) -- Export the newtype wrapper including runSQLiteRepo
    , withConn
    , migrate
    -- Direct functions for use in the presentation layer
    , selectAllTodos
    , selectTodoById
    , insertTodo
    , updateTodoById
    , deleteTodoById
    ) where

import           Control.Exception              (try)
import           Data.Text                      (pack)
import           Data.Time                      (getCurrentTime)
import           Database.SQLite.Simple
    ( Connection
    , Only (Only)
    , Query (Query)
    , close
    , execute
    , execute_
    , lastInsertRowId
    , open
    , query
    , query_
    )
import           Domain.Repositories.TodoRepository
    ( TodoRepository(..)
    , Todo (Todo, todoTitle)
    , NewTodo (newTodoName)
    , ValidationError (..)
    )
import           Domain.Repositories.Entities.Todo (validateTodoTitle, Priority(..))

-- -------------------------------------------------------------------
-- Infrastructure
-- -------------------------------------------------------------------

-- Newtype wrapper for SQLite IO operations
newtype SQLiteRepo a = SQLiteRepo { runSQLiteRepo :: IO a }
    deriving (Functor, Applicative, Monad)

-- TodoRepository implementation for SQLiteRepo
instance TodoRepository SQLiteRepo where
    getAllTodos = SQLiteRepo selectAllTodos
    getTodoById tid = SQLiteRepo $ selectTodoById tid
    createTodo newTodo = SQLiteRepo $ insertTodo newTodo
    updateTodo tid todo = SQLiteRepo $ updateTodoById tid todo
    deleteTodo tid = SQLiteRepo $ deleteTodoById tid

-- Connection helper
withConn :: (Connection -> IO a) -> IO a
withConn action = do
  conn <- open "test.db"
  a <- action conn
  close conn
  pure a

-- Database migration
migrate :: IO ()
migrate = withConn $ \conn -> do
  -- Drop the existing table if it exists (for schema migration)
  execute_ conn (Query $ pack "DROP TABLE IF EXISTS haskell_todo")
  -- Create the table with the new schema
  execute_ conn (Query $ pack "CREATE TABLE IF NOT EXISTS haskell_todo (todoId INTEGER PRIMARY KEY AUTOINCREMENT, todoTitle TEXT, createdAt TEXT, priority TEXT, isCompleted BOOLEAN)")

-- Direct functions for use in the presentation layer
-- Get all todos
selectAllTodos :: IO [Todo]
selectAllTodos = withConn $ \conn ->
  query_ conn (Query $ pack "SELECT todoId, todoTitle, createdAt, priority, isCompleted FROM haskell_todo")

-- Get a specific todo by ID
selectTodoById :: Int -> IO [Todo]
selectTodoById uId = withConn $ \conn ->
  query conn (Query $ pack "SELECT todoId, todoTitle, createdAt, priority, isCompleted FROM haskell_todo WHERE todoId = (?)") (Only uId)

-- Insert a new todo
insertTodo :: NewTodo -> IO (Either ValidationError [Todo])
insertTodo newTodo = do
  case validateTodoTitle (newTodoName newTodo) of
    Left err -> return $ Left err
    Right () -> do
      result <- try (insertTodoInDb newTodo) :: IO (Either IOError [Todo])
      case result of
        Left e -> return $ Left $ ValidationError $ pack $ "Database error: " ++ show e
        Right todos -> return $ Right todos
  where
    insertTodoInDb :: NewTodo -> IO [Todo]
    insertTodoInDb todo = withConn $ \conn -> do
      currentTime <- getCurrentTime
      -- Use Medium as the default priority
      execute conn (Query $ pack "INSERT INTO haskell_todo (todoTitle, createdAt, priority, isCompleted) VALUES (?, ?, ?, ?)") 
        (newTodoName todo, currentTime, "Medium", False)
      rowId <- lastInsertRowId conn
      query conn (Query $ pack "SELECT todoId, todoTitle, createdAt, priority, isCompleted FROM haskell_todo WHERE todoId = ?") (Only rowId)

-- Update an existing todo
updateTodoById :: Int -> Todo -> IO (Either ValidationError [Todo])
updateTodoById uId todo = do
  case validateTodoTitle (todoTitle todo) of
    Left err -> return $ Left err
    Right () -> do
      result <- try (updateTodoInDb uId todo) :: IO (Either IOError [Todo])
      case result of
        Left e -> return $ Left $ ValidationError $ pack $ "Database error: " ++ show e
        Right todos -> return $ Right todos
  where
    updateTodoInDb :: Int -> Todo -> IO [Todo]
    updateTodoInDb todoId' (Todo _ uName _ uPriority uIsCompleted) = withConn $ \conn -> do
      -- Convert Priority to string directly
      let priorityStr = case uPriority of
            Low -> "Low"
            Medium -> "Medium"
            High -> "High"
      _ <- execute conn (Query $ pack "UPDATE haskell_todo SET todoTitle = (?), priority = (?), isCompleted = (?) WHERE todoId = (?)") 
        (uName, priorityStr, uIsCompleted, todoId')
      query conn (Query $ pack "SELECT todoId, todoTitle, createdAt, priority, isCompleted FROM haskell_todo WHERE todoId = (?)") (Only todoId')

-- Delete a todo by ID
deleteTodoById :: Int -> IO [Todo]
deleteTodoById uId = withConn $ \conn -> do
  todo <- query conn (Query $ pack "SELECT todoId, todoTitle, createdAt, priority, isCompleted FROM haskell_todo WHERE todoId = (?)") (Only uId)
  execute conn (Query $ pack "DELETE FROM haskell_todo WHERE todoId = (?)") (Only uId)
  return todo


