module Application.UseCases.TodoUseCases
    ( getTodoList
    , getTodo
    , createNewTodo
    , updateExistingTodo
    , removeTodo
    ) where

import           Domain.Repositories.TodoRepository (TodoRepository(..), getAllTodos, getTodoById, createTodo, updateTodo, deleteTodo)
import           Domain.Repositories.Entities.Todo (Todo, NewTodo, ValidationError) 

-- -------------------------------------------------------------------
-- Use cases for Todo operations
-- -------------------------------------------------------------------

-- Get all todos
getTodoList :: (TodoRepository m) => m [Todo]
getTodoList = getAllTodos

-- Get a specific todo by ID
getTodo :: (TodoRepository m) => Int -> m [Todo]
getTodo = getTodoById

-- Create a new todo with validation
createNewTodo :: (TodoRepository m) => NewTodo -> m (Either ValidationError [Todo])
createNewTodo = createTodo

-- Update an existing todo with validation
updateExistingTodo :: (TodoRepository m) => Int -> Todo -> m (Either ValidationError [Todo])
updateExistingTodo = updateTodo

-- Remove a todo by ID
removeTodo :: (TodoRepository m) => Int -> m [Todo]
removeTodo = deleteTodo
