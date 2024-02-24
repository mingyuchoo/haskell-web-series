{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( someFunc
    ) where

-- -------------------------------------------------------------------

-- For web server
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson
import           Data.List                (partition)
import           Data.Maybe               (listToMaybe)
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

someFunc :: IO ()
someFunc = do
  migrate
  putStrLn "Server is running..."
  usersVar <- newMVar []
  run 4000 (app usersVar)


app :: MVar [User] -> Application
app usersVar = serve userAPI (userServer usersVar)

-- -------------------------------------------------------------------
-- API Server
-- -------------------------------------------------------------------

type UserAPI = "users" :> Get '[JSON] [User]
             :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User
             :<|> "users" :> Capture "userId" Int :> Get '[JSON] (Maybe User)
             :<|> "users" :> Capture "userId" Int :> ReqBody '[JSON] User :> Put '[JSON] User
             :<|> "usres" :> Capture "userId" Int :> Delete '[JSON] (Maybe User)

userAPI :: Proxy UserAPI
userAPI = Proxy

userServer :: MVar [User] -> Server UserAPI
userServer usersVar = getUsers :<|> postUser :<|> getUser :<|> putUser :<|> deleteUser
  where
    getUsers :: Handler [User]
    getUsers = liftIO $ readMVar usersVar

    postUser :: User -> Handler User
    postUser user = do
      liftIO $ modifyMVar_ usersVar $ \users ->
        return (user: users)
      return user

    getUser :: Int -> Handler (Maybe User)
    getUser uId = do
      users <- liftIO $ readMVar usersVar
      return $ listToMaybe $ filter (\user -> userId user == uId) users

    putUser :: Int -> User -> Handler User
    putUser uId updatedUser = do
      liftIO $ modifyMVar_ usersVar $ \users ->
        return $ map (\user ->
                        if userId user == uId then updatedUser else user) users
      return updatedUser

    deleteUser ::  Int -> Handler (Maybe User)
    deleteUser uId = do
      liftIO $ modifyMVar usersVar $ \users ->
        let
          (remain, removed) = partition (\user -> userId user == uId) users
        in
          return (remain, listToMaybe removed)


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


insert :: String -> Handler String
insert userName = liftIO $ withConn $ \conn -> do
  execute conn "INSERT INTO haskell_user (userName) VALUES (?)" (Only userName)
  pure userName

select :: String -> Handler [User]
select userName = liftIO $ withConn $ \conn ->
  query conn "SELECT userId, userName FROM haskell_user WHERE userName = (?)" (Only userName)
