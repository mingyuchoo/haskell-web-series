{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Presentation.API.TodoAPI
    ( TodoAPI
    , todoServer
    ) where

import           Control.Monad.IO.Class         (liftIO)
import           Domain.Repositories.Entities.Todo (Todo, NewTodo, ValidationError)
import           Application.UseCases.TodoUseCases (getTodoList, getTodo, createNewTodo, updateExistingTodo, removeTodo)
import           Infrastructure.Repositories.SQLiteTodoRepository (SQLiteIO)
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
    getAll = liftIO (getTodoList :: SQLiteIO [Todo])

    postOne :: NewTodo -> Handler (Either ValidationError [Todo])
    postOne newTodo = liftIO (createNewTodo newTodo :: SQLiteIO (Either ValidationError [Todo]))

    getOne :: Int -> Handler [Todo]
    getOne todoId = liftIO (getTodo todoId :: SQLiteIO [Todo])

    putOne :: Int -> Todo -> Handler (Either ValidationError [Todo])
    putOne uId todo = liftIO (updateExistingTodo uId todo :: SQLiteIO (Either ValidationError [Todo]))

    delOne ::  Int -> Handler [Todo]
    delOne todoId = liftIO (removeTodo todoId :: SQLiteIO [Todo])
