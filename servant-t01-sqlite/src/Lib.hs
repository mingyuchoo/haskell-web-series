{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( someFunc
    ) where

-- -------------------------------------------------------------------

-- For web server
import           Network.Wai.Handler.Warp (run)
import           Servant
    ( Application
    , Capture
    , Delete
    , Get
    , Handler
    , JSON
    , NoContent
    , Post
    , Proxy (Proxy)
    , Put
    , QueryParam
    , ReqBody
    , Server
    , serve
    , (:>)
    )

-- For Databaser
import           Control.Monad.IO.Class   (liftIO)
import           Database.SQLite.Simple
--     ( Connection
--     , FromRow (fromRow)
--     , ToRow (toRow)
--     , close
--     , execute
--     , execute_
--     , field
--     , open
--     , query_
--     )

-- For define types
import           Data.Aeson.Types         (FromJSON, ToJSON)
import           GHC.Generics

-- -------------------------------------------------------------------
-- Data Types
-- -------------------------------------------------------------------

data User = User { id   :: Int
                 , name :: String
                 } deriving (Eq, Show, Generic)

instance FromJSON User
instance ToJSON User

instance FromRow User where
  fromRow = User <$> field <*> field

instance ToRow User where
  toRow (User id name) = toRow (id, name)

-- -------------------------------------------------------------------
-- Application
-- -------------------------------------------------------------------

someFunc :: IO ()
someFunc = do
  migrate
  putStrLn "Server is running..."
  usersVar <- newMVar []
  _ <- run 4000 $ app usersVar
  return ()

-- -------------------------------------------------------------------
-- API Server
-- -------------------------------------------------------------------

type UserAPI =    "users" :> QueryParam "name" User :> Get '[JSON] [User]
             :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User
             :<|> "users" :> Capture "name" User :> Put '[JSON] User
             :<|> "usres" :> Capture "name" User :> Delete '[JSON] User

app :: MVar [User] -> Application
app usersVar = serve userAPI userServer
  where
    userAPI :: Proxy UserAPI
    userAPI = Proxy

    userServer :: MVar [User] -> Server UserAPI
    userServer usersVar = getUsers :<|> postUser :<|> putUser :<|> deleteUser
      where
        getUsers :: Maybe User -> Handler [User]
        getUsers user = do
          users <- liftIO $ readMVar usersVar
          return $ maybe users (\name -> lifter (== name) users) mName

        postUser :: Text -> Handler User
        postUser user = do
          liftIO $ modifyMVar_ usersVar $ \users -> return (name: users)
          return user

        putUser :: Text -> Text -> Handler Text
        putUser oldUser newUser = do
          liftIO $ modifyMVar_ userVar $ \users ->
            return $ map (\user ->
                            if user == oldUser then newUser else user) users

        deleteUser :: User -> Handler User
        deleteUser user = do
          liftIO $ modifyMVar_ usersVar $ \users -> return $ filter (/= name) user
          return name

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
  execute_ conn "CREATE TABLE IF NOT EXISTS haskell_user (id INTEGER PRIMARY KEY, name TEXT)"


insert :: String -> Handler String
insert name = liftIO $ withConn $ \conn -> do
  execute conn "INSERT INTO haskell_user (name) VALUES (?)" (Only name)
  pure name

select :: String -> Handler [User]
select name = liftIO $ withConn $ \conn ->
  query conn "SELECT id, name FROM haskell_user WHERE name = (?)" (Only name)
