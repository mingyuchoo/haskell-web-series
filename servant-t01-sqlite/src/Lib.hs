{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( appRunner
    ) where

-- -------------------------------------------------------------------

import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Text                (Text, pack)
import           Database.SQLite.Simple
    ( Connection
    , FromRow (..)
    , Only (Only)
    , ToRow (..)
    , close
    , execute
    , execute_
    , field
    , open
    , query
    , query_
    )
import           GHC.Generics             (Generic)
import           Lucid
import           Lucid.Base               (makeAttribute)
import           Network.HTTP.Media       ((//), (/:))
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
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
    , ServerT
    , serveDirectoryWith
    , serve
    , type (:<|>) (..)
    , type (:>)
    )
import           Servant.HTML.Lucid       (HTML)

-- -------------------------------------------------------------------
-- Data
-- -------------------------------------------------------------------

data User = User { userId   :: Int
                 , userName :: String
                 } deriving (Eq, Show, Generic)

instance FromJSON User

instance ToJSON User

instance FromRow User where
  fromRow = User <$> field <*> field

instance ToRow User where
  toRow (User userId userName) = toRow (userId, userName)

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
indexTemplate :: [User] -> Html ()
indexTemplate users = baseTemplate "User Management" mempty $ do
  div_ [id_ "message-container"] mempty
  
  -- User form
  div_ [class_ "container"] $ do
    h2_ [id_ "form-title"] "Create New User"
    form_ [id_ "user-form"] $ do
      input_ [type_ "hidden", id_ "form-mode", name_ "form-mode", value_ "create"]
      div_ [class_ "form-group"] $ do
        label_ [for_ "userId"] "User ID:"
        input_ [type_ "number", id_ "userId", name_ "userId", required_ "required"]
      div_ [class_ "form-group"] $ do
        label_ [for_ "userName"] "User Name:"
        input_ [type_ "text", id_ "userName", name_ "userName", required_ "required"]
      button_ [type_ "submit", class_ "btn btn-success", id_ "submit-btn"] "Create User"
      button_ [type_ "button", class_ "btn", onclick_ "resetForm()"] "Reset"
  
  -- Users table
  div_ [class_ "container"] $ do
    h2_ "User List"
    table_ $ do
      thead_ $ do
        tr_ $ do
          th_ "ID"
          th_ "Name"
          th_ "Actions"
      tbody_ [id_ "users-table-body"] $ do
        if null users
          then tr_ $ td_ [colspan_ "3"] "No users found"
          else mapM_ userRow users

-- Single user row template
userRow :: User -> Html ()
userRow user = tr_ $ do
  td_ (toHtml $ show $ userId user)
  td_ (toHtml $ userName user)
  td_ $ do
    button_ [class_ "btn", onclick_ $ "editUser(" <> pack (show $ userId user) <> ", '" <> pack (userName user) <> "')"]
      "Edit"
    button_ [class_ "btn btn-danger", onclick_ $ "deleteUser(" <> pack (show $ userId user) <> ")"]
      "Delete"

-- -------------------------------------------------------------------
-- Application
-- -------------------------------------------------------------------

appRunner :: IO ()
appRunner = do
  migrate
  putStrLn "Server is started at port: 4000"
  run 4000 app


app :: Application
app = serve appAPI appServer

-- -------------------------------------------------------------------
-- API
-- -------------------------------------------------------------------

-- API for JSON endpoints
type UserAPI = "users" :> Get '[JSON] [User]
             :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] [User]
             :<|> "users" :> Capture "userId" Int :> Get  '[JSON] [User]
             :<|> "users" :> Capture "userId" Int :> ReqBody '[JSON] User :> Put '[JSON] [User]
             :<|> "users" :> Capture "userId" Int :> Delete '[JSON] [User]

-- API for HTML web interface
type WebAPI = Get '[HTML] (Html ())
          :<|> "static" :> Raw

-- Combined API
type AppAPI = WebAPI :<|> UserAPI

appAPI :: Proxy AppAPI
appAPI = Proxy

-- Server implementation
appServer :: Server AppAPI
appServer = webServer :<|> userServer

-- Web interface server
webServer :: Server WebAPI
webServer = indexHandler :<|> serveDirectoryWith (defaultWebAppSettings "static")
  where
    indexHandler :: Handler (Html ())
    indexHandler = do
      users <- liftIO selectAllUser
      return $ indexTemplate users

-- JSON API server
userServer :: Server UserAPI
userServer = getAll
  :<|> postOne
  :<|> getOne
  :<|> putOne
  :<|> delOne
  where
    getAll :: Handler [User]
    getAll = liftIO selectAllUser

    postOne :: User -> Handler [User]
    postOne = liftIO . insertOneUser

    getOne :: Int -> Handler [User]
    getOne = liftIO . selectOneUser

    putOne :: Int -> User -> Handler [User]
    putOne uId user =  liftIO $ updateOneUser uId user

    delOne ::  Int -> Handler [User]
    delOne uId = liftIO $ deleteOneUser uId

-- -------------------------------------------------------------------
-- Database
-- -------------------------------------------------------------------

withConn :: (Connection -> IO a) -> IO a
withConn action = do
  conn <- open "test.db"
  a <- action conn
  close conn
  pure a

migrate :: IO ()
migrate = withConn $ \conn ->
  execute_ conn "CREATE TABLE IF NOT EXISTS haskell_user (userId INTEGER PRIMARY KEY, userName TEXT)"

insertOneUser :: User -> IO [User]
insertOneUser user = withConn $ \conn -> do
  _ <- execute conn "INSERT INTO haskell_user (userId, userName) VALUES (?, ?)" user

  query conn "SELECT userId, userName FROM haskell_user WHERE userId = (?) AND userName = (?)" user

selectOneUser :: Int -> IO [User]
selectOneUser uId = withConn $ \conn ->
  query conn "SELECT userId, userName FROM haskell_user WHERE userId = (?)" (Only uId)

selectAllUser :: IO [User]
selectAllUser = withConn $ \conn ->
  query_ conn "SELECT userId, userName FROM haskell_user"

updateOneUser :: Int -> User -> IO [User]
updateOneUser uId user@(User _ uName) = withConn $ \conn -> do
  _ <- execute conn "UPDATE haskell_user SET userName = (?) WHERE userId = (?)" (uName, uId)
  query conn "SELECT userId, userName FROM haskell_user WHERE userId = (?) AND userName = (?)" user

deleteOneUser :: Int -> IO [User]
deleteOneUser uId = withConn $ \conn -> do
  user <- query conn "SELECT userId, userName FROM haskell_user WHERE userId = (?)" (Only uId)
  execute conn "DELETE FROM haskell_user WHERE userId = (?)" (Only uId)
  return user
