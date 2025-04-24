{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Presentation.Web.WebAPI
    ( WebAPI
    , webServer
    ) where

import           Control.Monad.IO.Class         (liftIO)
import           Domain.Repositories.Entities.Todo           (Todo)
import Domain.Repositories.TodoRepository ( getAllTodos )
import           Infrastructure.Repositories.SQLiteTodoRepository (SQLiteRepo(..))
import           Network.Wai.Application.Static (defaultWebAppSettings)
import           Presentation.Web.Templates      (indexTemplate)
import           Servant
    ( Get
    , Handler
    , Raw
    , Server
    , serveDirectoryWith
    , (:<|>) (..)
    , (:>)
    )
import           Servant.HTML.Lucid             (HTML)
import           Lucid                          (Html)

-- -------------------------------------------------------------------
-- Web APIs
-- -------------------------------------------------------------------

-- API type definition for web interface
type WebAPI = Get '[HTML] (Html ())
          :<|> "static" :> Raw

-- Web interface server implementation
webServer :: Server WebAPI
webServer = indexHandler :<|> staticFiles
  where
    indexHandler :: Handler (Html ())
    indexHandler = do
      todos <- liftIO $ runSQLiteRepo (getAllTodos :: SQLiteRepo [Todo])
      return $ indexTemplate todos

    staticFiles :: Server Raw
    staticFiles = serveDirectoryWith (defaultWebAppSettings "static")
