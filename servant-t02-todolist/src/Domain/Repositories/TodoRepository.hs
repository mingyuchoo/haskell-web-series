module Domain.Repositories.TodoRepository
    ( -- Repository interface
      TodoRepository(..)
      -- Re-export everything from Domain.Repositories.Entities.Todo
    , module Domain.Repositories.Entities.Todo
    ) where

import           Domain.Repositories.Entities.Todo

-- -------------------------------------------------------------------
-- Repository
-- -------------------------------------------------------------------
    
-- Repository interface defining operations that can be performed on Todo entities
class TodoRepository m where
    getAllTodos :: m [Todo]
    getTodoById :: Int -> m [Todo]
    createTodo :: NewTodo -> m (Either ValidationError [Todo])
    updateTodo :: Int -> Todo -> m (Either ValidationError [Todo])
    deleteTodo :: Int -> m [Todo]
