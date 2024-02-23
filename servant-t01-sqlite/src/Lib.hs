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
    , Get
    , Handler
    , JSON
    , Proxy (Proxy)
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
import           Data.Aeson.Types         (ToJSON)
import           GHC.Generics

-- -------------------------------------------------------------------

data User = User { id   :: Int
                 , name :: String
                 } deriving (Eq, Show, Generic)

instance ToJSON User

instance FromRow User where
  fromRow = User <$> field <*> field

instance ToRow User where
  toRow (User id name) = toRow (id, name)

-- -------------------------------------------------------------------

someFunc :: IO ()
someFunc = do
  migrate
  putStrLn "Server is running..."
  _ <- run 4000 app
  return ()

-- -------------------------------------------------------------------
--
-- serve :: HasServer api '[] => Proxy api -> Server api -> Application
--

type UserAPI = "users" :> Get '[JSON] [User]

app :: Application
app = serve userAPI server
  where
    userAPI :: Proxy UserAPI
    userAPI = Proxy

    server :: Server UserAPI
    -- server = return [User 1 "Alex"]
    server = do
      insert "Alex"
      select

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
  execute conn "INSERT INTO haskell_user (name) VALUES (?)" (Only (name))
  pure name

select :: Handler [User]
select = liftIO $ withConn $ \conn ->
  query_ conn "SELECT id, name FROM haskell_user"
