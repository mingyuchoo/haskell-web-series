{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( appRunner
    ) where

-- -------------------------------------------------------------------

import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (FromJSON, ToJSON)
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
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant
    ( Capture
    , Delete
    , Get
    , Handler
    , JSON
    , Post
    , Proxy (..)
    , Put
    , ReqBody
    , Server
    , serve
    , type (:<|>) (..)
    , type (:>)
    )

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
-- Application
-- -------------------------------------------------------------------

appRunner :: IO ()
appRunner = do
  migrate
  putStrLn "Server is running..."
  run 4000 app


app :: Application
app = serve userAPI userServer

-- -------------------------------------------------------------------
-- API
-- -------------------------------------------------------------------

type UserAPI = "users" :> Get '[JSON] [User]
             :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] [User]
             :<|> "users" :> Capture "userId" Int :> Get  '[JSON] [User]
             :<|> "users" :> Capture "userId" Int :> ReqBody '[JSON] User :> Put '[JSON] [User]
             :<|> "users" :> Capture "userId" Int :> Delete '[JSON] [User]

userAPI :: Proxy UserAPI
userAPI = Proxy

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
