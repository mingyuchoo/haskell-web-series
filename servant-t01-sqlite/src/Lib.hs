{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( appRunner
    ) where

-- -------------------------------------------------------------------

import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson
import           Database.SQLite.Simple
import           GHC.Generics             (Generic)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

-- -------------------------------------------------------------------
-- Data Types
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
userServer = getUsers
  :<|> postUser
  :<|> getUser
  :<|> putUser
  :<|> deleteUser
  where
    getUsers :: Handler [User]
    getUsers = liftIO selectAll

    postUser :: User -> Handler [User]
    postUser = liftIO . insert

    getUser :: Int -> Handler [User]
    getUser = liftIO . select

    putUser :: Int -> User -> Handler [User]
    putUser uId user =  liftIO $ update uId user

    deleteUser ::  Int -> Handler [User]
    deleteUser uId = liftIO $ delete uId

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

insert :: User -> IO [User]
insert user = withConn $ \conn -> do
  _ <- execute conn "INSERT INTO haskell_user (userId, userName) VALUES (?, ?)" user

  query conn "SELECT userId, userName FROM haskell_user WHERE userId = (?) AND userName = (?)" user

select :: Int -> IO [User]
select uId = withConn $ \conn ->
  query conn "SELECT userId, userName FROM haskell_user WHERE userId = (?)" (Only uId)

selectAll :: IO [User]
selectAll = withConn $ \conn ->
  query_ conn "SELECT userId, userName FROM haskell_user"

update :: Int -> User -> IO [User]
update uId user@(User _ uName) = withConn $ \conn -> do
  _ <- execute conn "UPDATE haskell_user SET userName = (?) WHERE userId = (?)" (uName, uId)
  query conn "SELECT userId, userName FROM haskell_user WHERE userId = (?) AND userName = (?)" user

delete :: Int -> IO [User]
delete uId = withConn $ \conn -> do
  user <- query conn "SELECT userId, userName FROM haskell_user WHERE userId = (?)" (Only uId)
  execute conn "DELETE FROM haskell_user WHERE userId = (?)" (Only uId)
  return user
