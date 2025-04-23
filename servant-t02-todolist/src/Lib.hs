{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( appRunner
    ) where

-- -------------------------------------------------------------------

import           Network.Wai                                     (Application)
import           Network.Wai.Handler.Warp                        (run)
import           Servant
    ( Proxy (..)
    , Server
    , serve
    , type (:<|>) (..)
    )

-- Import domain, application, and infrastructure layers
import           Infrastructure.Persistence.SQLiteTodoRepository (migrate)

-- Import presentation layer
import           Presentation.API.TodoAPI
    ( TodoAPI
    , todoServer
    )
import           Presentation.Web.WebAPI
    ( WebAPI
    , webServer
    )

-- Import instances for the combined API type
import           Domain.Entities.Todo                            ()
import           Lucid                                           ()
import           Servant.HTML.Lucid                              ()



-- -------------------------------------------------------------------
-- Application
-- -------------------------------------------------------------------

-- Create the WAI application
app :: Application
app = serve appAPI appServer

-- Run the application
appRunner :: IO ()
appRunner = do
  putStrLn "Starting server on port 4000..."
  migrate  -- Run database migration
  run 4000 app  -- Start the server

-- -------------------------------------------------------------------
-- API
-- -------------------------------------------------------------------

-- Combined API types are imported from the presentation layer modules

-- Combined API
type AppAPI = TodoAPI :<|> WebAPI

-- API proxy
appAPI :: Proxy AppAPI
appAPI = Proxy

-- Server implementation
appServer :: Server AppAPI
appServer = todoServer :<|> webServer
