{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module Infrastructure.Persistence.SQLiteTodoRepository
    ( SQLiteIO
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
import           Domain.Entities.Todo           (Todo(..), NewTodo(..), ValidationError(..), validateTodoTitle)
import           Domain.Repositories.TodoRepository (TodoRepository(..))

-- Type for SQLite IO operations
type SQLiteIO = IO

-- Connection helper
withConn :: (Connection -> IO a) -> IO a
withConn action = do
  conn <- open "test.db"
  a <- action conn
  close conn
  pure a

-- Database migration
migrate :: IO ()
migrate = withConn $ \conn ->
  execute_ conn (Query $ pack "CREATE TABLE IF NOT EXISTS haskell_todo (todoId INTEGER PRIMARY KEY AUTOINCREMENT, todoTitle TEXT)")

-- Direct functions for use in the presentation layer
-- Get all todos
selectAllTodos :: IO [Todo]
selectAllTodos = withConn $ \conn ->
  query_ conn (Query $ pack "SELECT todoId, todoTitle FROM haskell_todo")

-- Get a specific todo by ID
selectTodoById :: Int -> IO [Todo]
selectTodoById uId = withConn $ \conn ->
  query conn (Query $ pack "SELECT todoId, todoTitle FROM haskell_todo WHERE todoId = (?)") (Only uId)

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
      execute conn (Query $ pack "INSERT INTO haskell_todo (todoTitle) VALUES (?)") (Only (newTodoName todo))
      rowId <- lastInsertRowId conn
      query conn (Query $ pack "SELECT todoId, todoTitle FROM haskell_todo WHERE todoId = ?") (Only rowId)

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
    updateTodoInDb todoId' (Todo _ uName) = withConn $ \conn -> do
      _ <- execute conn (Query $ pack "UPDATE haskell_todo SET todoTitle = (?) WHERE todoId = (?)") (uName, todoId')
      query conn (Query $ pack "SELECT todoId, todoTitle FROM haskell_todo WHERE todoId = (?) AND todoTitle = (?)") (todoId', uName)

-- Delete a todo by ID
deleteTodoById :: Int -> IO [Todo]
deleteTodoById uId = withConn $ \conn -> do
  todo <- query conn (Query $ pack "SELECT todoId, todoTitle FROM haskell_todo WHERE todoId = (?)") (Only uId)
  execute conn (Query $ pack "DELETE FROM haskell_todo WHERE todoId = (?)") (Only uId)
  return todo

-- Implementation of TodoRepository for SQLiteIO
-- Note: This is an orphan instance, but we're keeping it here for simplicity
-- In a production environment, we would either:
-- 1. Move this instance to the TodoRepository module, or
-- 2. Create a newtype wrapper around SQLiteIO and define the instance for that
instance TodoRepository SQLiteIO where
    getAllTodos = selectAllTodos
    getTodoById = selectTodoById
    createTodo = insertTodo
    updateTodo = updateTodoById
    deleteTodo = deleteTodoById
