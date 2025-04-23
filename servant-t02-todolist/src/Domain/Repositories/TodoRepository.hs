module Domain.Repositories.TodoRepository
    ( TodoRepository(..) 
    ) where

import           Domain.Entities.Todo           (Todo, NewTodo, ValidationError)

-- Repository interface defining operations that can be performed on Todo entities
class TodoRepository m where
    getAllTodos :: m [Todo]
    getTodoById :: Int -> m [Todo]
    createTodo :: NewTodo -> m (Either ValidationError [Todo])
    updateTodo :: Int -> Todo -> m (Either ValidationError [Todo])
    deleteTodo :: Int -> m [Todo]
