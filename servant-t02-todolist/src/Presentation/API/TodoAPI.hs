{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Presentation.API.TodoAPI
    ( TodoAPI
    , todoServer
    ) where

import           Control.Monad.IO.Class         (liftIO)
import           Domain.Entities.Todo           (Todo, NewTodo, ValidationError)
import           Infrastructure.Persistence.SQLiteTodoRepository
                                                (selectAllTodos, selectTodoById, insertTodo, updateTodoById, deleteTodoById)
import           Servant
    ( Capture
    , Delete
    , Get
    , Handler
    , JSON
    , Post
    , Put
    , ReqBody
    , Server
    , type (:<|>) (..)
    , type (:>)
    )

-- -------------------------------------------------------------------
-- APIs
-- -------------------------------------------------------------------


-- API type definition
type TodoAPI = "api" :> "todos" :> Get '[JSON] [Todo]
          :<|> "api" :> "todos" :> ReqBody '[JSON] NewTodo :> Post '[JSON] (Either ValidationError [Todo])
          :<|> "api" :> "todos" :> Capture "todoId" Int :> Get '[JSON] [Todo]
          :<|> "api" :> "todos" :> Capture "todoId" Int :> ReqBody '[JSON] Todo :> Put '[JSON] (Either ValidationError [Todo])
          :<|> "api" :> "todos" :> Capture "todoId" Int :> Delete '[JSON] [Todo]

-- API server implementation
todoServer :: Server TodoAPI
todoServer = getAll
  :<|> postOne
  :<|> getOne
  :<|> putOne
  :<|> delOne
  where
    getAll :: Handler [Todo]
    getAll = liftIO selectAllTodos

    postOne :: NewTodo -> Handler (Either ValidationError [Todo])
    postOne = liftIO . insertTodo

    getOne :: Int -> Handler [Todo]
    getOne = liftIO . selectTodoById

    putOne :: Int -> Todo -> Handler (Either ValidationError [Todo])
    putOne uId = liftIO . updateTodoById uId

    delOne ::  Int -> Handler [Todo]
    delOne = liftIO . deleteTodoById
