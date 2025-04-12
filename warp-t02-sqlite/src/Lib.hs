{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( appRunner
    ) where

import           Control.Exception         (bracket)
import           Data.Aeson                (encode, decode, object)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Lazy      as LBS
import           Data.Kind                 ()
import qualified Data.Text                 as T
import           Database                  (User(..), initDB, createUser, getUser, getUsers, updateUser, deleteUser)
import           Database.SQLite.Simple     (Connection)
import           Flow                      ((<|))
import           Network.HTTP.Types
    ( Status
    , methodDelete
    , methodGet
    , methodPost
    , methodPut
    , status200
    , status201
    , status204
    , status400
    , status404
    )
import           Network.HTTP.Types.Header (hContentType)
import           Network.Wai
    ( Request (pathInfo, requestMethod)
    , Response
    , ResponseReceived
    , responseFile
    , responseLBS
    , strictRequestBody
    )
import           Network.Wai.Handler.Warp  (run)
import           Text.Read                 (readMaybe)

-- | Main Function
--
appRunner :: IO ()
appRunner = do
  putStrLn <| "listening on " <> show port
  bracket initDB (\_ -> putStrLn "Closing database connection") $ \conn -> do
    run port (app conn)
  where
    port :: Int
    port = 4000

-- | Application
--
app :: Connection -> Request                           -- ^ request
    -> (Response -> IO ResponseReceived) -- ^ handler response to IO
    -> IO ResponseReceived               -- ^ response
app conn request respond = do
  case requestMethod request of
    -- GET requests
    method | method == methodGet -> do
      let reqPathInfo = pathInfo request
      case reqPathInfo of
        [] -> respond index
        ["api", "users"] -> do
          users <- getUsers conn
          respond $ jsonResponse status200 $ encode users
        ["api", "users", userId] -> do
          case readMaybe (T.unpack userId) of
            Just id' -> do
              maybeUser <- getUser conn id'
              case maybeUser of
                Just user -> respond $ jsonResponse status200 $ encode user
                Nothing -> respond $ jsonResponse status404 $ encode (object [("error", "User not found")])
            Nothing -> respond $ jsonResponse status400 $ encode (object [("error", "Invalid user ID")])
        _ -> respond $ responseFile status200 [(hContentType, "text/html"), ("Access-Control-Allow-Origin", "*")] "www/index.html" Nothing

    -- POST requests
    method | method == methodPost -> do
      let reqPathInfo = pathInfo request
      case reqPathInfo of
        ["api", "users"] -> do
          body <- strictRequestBody request
          case decode body of
            Just user -> do
              newUser <- createUser conn user
              respond $ jsonResponse status201 $ encode newUser
            Nothing -> respond $ jsonResponse status400 $ encode (object [("error", "Invalid user data")])
        _ -> respond $ jsonResponse status404 $ encode (object [("error", "Endpoint not found")])

    -- PUT requests
    method | method == methodPut -> do
      let reqPathInfo = pathInfo request
      case reqPathInfo of
        ["api", "users", userId] -> do
          case readMaybe (T.unpack userId) of
            Just id' -> do
              body <- strictRequestBody request
              case decode body of
                Just user -> do
                  let userWithId = user { userId = Just id' }
                  success <- updateUser conn userWithId
                  if success
                    then respond $ jsonResponse status200 $ encode userWithId
                    else respond $ jsonResponse status404 $ encode (object [("error", "User not found")])
                Nothing -> respond $ jsonResponse status400 $ encode (object [("error", "Invalid user data")])
            Nothing -> respond $ jsonResponse status400 $ encode (object [("error", "Invalid user ID")])
        _ -> respond $ jsonResponse status404 $ encode (object [("error", "Endpoint not found")])

    -- DELETE requests
    method | method == methodDelete -> do
      let reqPathInfo = pathInfo request
      case reqPathInfo of
        ["api", "users", userId] -> do
          case readMaybe (T.unpack userId) of
            Just id' -> do
              success <- deleteUser conn id'
              if success
                then respond $ jsonResponse status204 $ encode (object [])
                else respond $ jsonResponse status404 $ encode (object [("error", "User not found")])
            Nothing -> respond $ jsonResponse status400 $ encode (object [("error", "Invalid user ID")])
        _ -> respond $ jsonResponse status404 $ encode (object [("error", "Endpoint not found")])

    -- Other methods
    _ -> respond $ jsonResponse status404 $ encode (object [("error", "Method not supported")])

-- | CORS header
-- | JSON Response helper
jsonResponse :: Status -> LBS.ByteString -> Response
jsonResponse status = responseLBS status [(hContentType, "application/json"), ("Access-Control-Allow-Origin", "*")]

-- | GET / Index Page
--
index :: Response
index = responseFile status200 [(hContentType, "text/html"), ("Access-Control-Allow-Origin", "*")] "www/index.html" Nothing
