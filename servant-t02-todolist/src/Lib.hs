{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( appRunner
    ) where

-- -------------------------------------------------------------------

import           Control.Exception              (try)
import           Control.Monad.IO.Class         (liftIO)
import           Data.Aeson                     (FromJSON, ToJSON)
import           Data.Text                      (Text, pack)
import           Database.SQLite.Simple
    ( Connection
    , FromRow (..)
    , Only (Only)
    , ToRow (..)
    , close
    , execute
    , execute_
    , field
    , lastInsertRowId
    , open
    , query
    , query_
    )
import           GHC.Generics                   (Generic)
import           Lucid
import           Network.Wai                    (Application)
import           Network.Wai.Application.Static (defaultWebAppSettings)
import           Network.Wai.Handler.Warp       (run)
import           Servant
    ( Capture
    , Delete
    , Get
    , Handler
    , JSON
    , Post
    , Proxy (..)
    , Put
    , Raw
    , ReqBody
    , Server
    , serve
    , serveDirectoryWith
    , type (:<|>) (..)
    , type (:>)
    )
import           Servant.HTML.Lucid             (HTML)

-- -------------------------------------------------------------------
-- Data
-- -------------------------------------------------------------------

data Todo = Todo { todoId   :: Int
                 , todoTitle :: String
                 }
     deriving (Eq, Generic, Show)

-- Used for creating a new todo without specifying todoId
newtype NewTodo = NewTodo { newTodoName :: String }
     deriving (Eq, Generic, Show)

instance FromJSON NewTodo

instance ToJSON NewTodo

-- Validation error response
newtype ValidationError = ValidationError { errorMessage :: Text }
     deriving (Eq, Generic, Show)

instance ToJSON ValidationError

instance FromJSON Todo

instance ToJSON Todo

instance FromRow Todo where
  fromRow = Todo <$> field <*> field

instance ToRow Todo where
  toRow (Todo todoId todoTitle) = toRow (todoId, todoTitle)

-- -------------------------------------------------------------------
-- HTML Templates
-- -------------------------------------------------------------------

-- Base template with common elements
baseTemplate :: Text -> Html () -> Html () -> Html ()
baseTemplate title headContent bodyContent = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    title_ (toHtml title)
    link_ [rel_ "stylesheet", href_ "/static/css/style.css"]
    headContent
  body_ $ do
    div_ [class_ "container"] $ do
      h1_ (toHtml title)
      bodyContent
    script_ [src_ "/static/js/main.js"] ("" :: Text)

-- Index page template
indexTemplate :: [Todo] -> Html ()
indexTemplate todos = baseTemplate "Todo Management" mempty $ do
  div_ [id_ "message-container"] mempty

  -- Todo form
  div_ [class_ "container"] $ do
    h2_ [id_ "form-title"] "Create New Todo"
    form_ [id_ "todo-form"] $ do
      input_ [type_ "hidden", id_ "form-mode", name_ "form-mode", value_ "create"]
      input_ [type_ "hidden", id_ "todoId", name_ "todoId"]
      div_ [class_ "form-group"] $ do
        label_ [for_ "todoTitle"] "Todo Name:"
        input_ [type_ "text", id_ "todoTitle", name_ "todoTitle", required_ "required"]
      button_ [type_ "submit", class_ "btn btn-success", id_ "submit-btn"] "Create Todo"
      button_ [type_ "button", class_ "btn", onclick_ "resetForm()"] "Reset"

  -- Todos table
  div_ [class_ "container"] $ do
    h2_ "Todo List"
    table_ $ do
      thead_ $ do
        tr_ $ do
          th_ "ID"
          th_ "Name"
          th_ "Actions"
      tbody_ [id_ "todos-table-body"] $ do
        if null todos
          then tr_ $ td_ [colspan_ "3"] "No todos found"
          else mapM_ todoRow todos

-- Single todo row template
todoRow :: Todo -> Html ()
todoRow todo = tr_ $ do
  td_ (toHtml $ show $ todoId todo)
  td_ (toHtml $ todoTitle todo)
  td_ $ do
    button_ [class_ "btn", onclick_ $ "editTodo(" <> pack (show $ todoId todo) <> ", '" <> pack (todoTitle todo) <> "')"]
      "Edit"
    button_ [class_ "btn btn-danger", onclick_ $ "deleteTodo(" <> pack (show $ todoId todo) <> ")"]
      "Delete"

-- -------------------------------------------------------------------
-- Application
-- -------------------------------------------------------------------

-- Application runner
appRunner :: IO ()
appRunner = do
  migrate
  putStrLn "Server is started at port: 4000"
  run 4000 app

-- Application server
app :: Application
app = serve appAPI appServer

-- -------------------------------------------------------------------
-- API
-- -------------------------------------------------------------------

-- API for JSON endpoints
type TodoAPI = "todos" :> Get '[JSON] [Todo]
             :<|> "todos" :> ReqBody '[JSON] NewTodo :> Post '[JSON] (Either ValidationError [Todo])
             :<|> "todos" :> Capture "todoId" Int :> Get  '[JSON] [Todo]
             :<|> "todos" :> Capture "todoId" Int :> ReqBody '[JSON] Todo :> Put '[JSON] (Either ValidationError [Todo])
             :<|> "todos" :> Capture "todoId" Int :> Delete '[JSON] [Todo]

-- API for HTML web interface
type WebAPI = Get '[HTML] (Html ())
          :<|> "static" :> Raw

-- Combined API
type AppAPI = WebAPI :<|> TodoAPI

-- API proxy
appAPI :: Proxy AppAPI
appAPI = Proxy

-- Server implementation
appServer :: Server AppAPI
appServer = webServer :<|> todoServer

-- Web interface server
webServer :: Server WebAPI
webServer = indexHandler :<|> serveDirectoryWith (defaultWebAppSettings "static")
  where
    indexHandler :: Handler (Html ())
    indexHandler = do
      todos <- liftIO selectAllTodo
      return $ indexTemplate todos

-- JSON API server
todoServer :: Server TodoAPI
todoServer = getAll
  :<|> postOne
  :<|> getOne
  :<|> putOne
  :<|> delOne
  where
    getAll :: Handler [Todo]
    getAll = liftIO selectAllTodo

    postOne :: NewTodo -> Handler (Either ValidationError [Todo])
    postOne newTodo = liftIO $ validateAndInsertTodo newTodo

    getOne :: Int -> Handler [Todo]
    getOne = liftIO . selectOneTodo

    putOne :: Int -> Todo -> Handler (Either ValidationError [Todo])
    putOne uId todo = liftIO $ validateAndUpdateTodo uId todo

    delOne ::  Int -> Handler [Todo]
    delOne uId = liftIO $ deleteOneTodo uId

-- -------------------------------------------------------------------
-- Database
-- -------------------------------------------------------------------

withConn :: (Connection -> IO a) -> IO a
withConn action = do
  conn <- open "test.db"
  a <- action conn
  close conn
  pure a

-- Migration
migrate :: IO ()
migrate = withConn $ \conn ->
  execute_ conn "CREATE TABLE IF NOT EXISTS haskell_todo (todoId INTEGER PRIMARY KEY AUTOINCREMENT, todoTitle TEXT)"

-- Original insert function
insertOneTodo :: NewTodo -> IO [Todo]
insertOneTodo newTodo = withConn $ \conn -> do
  execute conn "INSERT INTO haskell_todo (todoTitle) VALUES (?)" (Only (newTodoName newTodo))
  rowId <- lastInsertRowId conn
  query conn "SELECT todoId, todoTitle FROM haskell_todo WHERE todoId = ?" (Only rowId)

-- Original select functions
selectOneTodo :: Int -> IO [Todo]
selectOneTodo uId = withConn $ \conn ->
  query conn "SELECT todoId, todoTitle FROM haskell_todo WHERE todoId = (?)" (Only uId)

-- Original select all function
selectAllTodo :: IO [Todo]
selectAllTodo = withConn $ \conn ->
  query_ conn "SELECT todoId, todoTitle FROM haskell_todo"

-- Original update function
updateOneTodo :: Int -> Todo -> IO [Todo]
updateOneTodo uId todo@(Todo _ uName) = withConn $ \conn -> do
  _ <- execute conn "UPDATE haskell_todo SET todoTitle = (?) WHERE todoId = (?)" (uName, uId)
  query conn "SELECT todoId, todoTitle FROM haskell_todo WHERE todoId = (?) AND todoTitle = (?)" todo

-- Original delete function
deleteOneTodo :: Int -> IO [Todo]
deleteOneTodo uId = withConn $ \conn -> do
  todo <- query conn "SELECT todoId, todoTitle FROM haskell_todo WHERE todoId = (?)" (Only uId)
  execute conn "DELETE FROM haskell_todo WHERE todoId = (?)" (Only uId)
  return todo

-- Validation functions
validateTodoTitle :: String -> Either ValidationError ()
validateTodoTitle title
  | null title = Left $ ValidationError "TodoTitle cannot be empty"
  | length title < 3 = Left $ ValidationError "TodoTitle must be at least 3 characters long"
  | length title > 50 = Left $ ValidationError "TodoTitle must be at most 50 characters long"
  | not (all (\c -> c `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', '_', '-']) title) =
      Left $ ValidationError "TodoTitle can only contain letters, numbers, spaces, underscores, and hyphens"
  | otherwise = Right ()

-- Insert with validation
validateAndInsertTodo :: NewTodo -> IO (Either ValidationError [Todo])
validateAndInsertTodo newTodo = do
  case validateTodoTitle (newTodoName newTodo) of
    Left err -> return $ Left err
    Right () -> do
      result <- try (insertOneTodo newTodo) :: IO (Either IOError [Todo])
      case result of
        Left e -> return $ Left $ ValidationError $ pack $ "Database error: " ++ show e
        Right todos -> return $ Right todos

-- Update with validation
validateAndUpdateTodo :: Int -> Todo -> IO (Either ValidationError [Todo])
validateAndUpdateTodo uId todo = do
  case validateTodoTitle (todoTitle todo) of
    Left err -> return $ Left err
    Right () -> do
      result <- try (updateOneTodo uId todo) :: IO (Either IOError [Todo])
      case result of
        Left e -> return $ Left $ ValidationError $ pack $ "Database error: " ++ show e
        Right todos -> return $ Right todos

