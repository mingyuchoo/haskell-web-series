{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Lib
    where

import           Data.Aeson                ()
import           Data.ByteString           (ByteString, length)
import           Data.ByteString.Lazy      (fromStrict)
import           Data.Kind                 ()
import           Flow                      ((<|))
import           Network.HTTP.Types
    ( methodDelete
    , methodGet
    , methodPost
    , methodPut
    , status200
    )
import           Network.HTTP.Types.Header (hContentType)
import           Network.HTTP.Types.URI    (Query)
import           Network.Wai
    ( Request
    , Response
    , ResponseReceived
    , pathInfo
    , queryString
    , rawPathInfo
    , requestMethod
    , responseFile
    , responseLBS
    )
import           Network.Wai.Handler.Warp  (run)


-- | Main Function
--
--
someFunc :: IO ()
someFunc = do
  putStrLn <| "listening on " <> show port
  run port app
  where
    port :: Int
    port = 3000


-- | Application
--
--
app :: Request                           -- ^ request
    -> (Response -> IO ResponseReceived) -- ^ handler response to IO
    -> IO ResponseReceived               -- ^ response
app request respond
  | requestMethod request == methodPost   = respond <| post
  | requestMethod request == methodPut    = respond <| put
  | requestMethod request == methodDelete = respond <| delete
  | requestMethod request == methodGet  =
    case (reqPathInfo, reqQueryString) of
      ([], _)                         -> respond <| index
      (["expr"], [("q", Just stuff)]) -> respond <| homeRoute stuff
      _                               -> respond <| notFoundRoute
    where
      reqPathInfo = pathInfo request
      reqQueryString = queryString request


-- | Index Page
--
index :: Response
index =
  responseFile status200 [(hContentType, "text/html")] "www/index.html" Nothing


-- | JSON Response
--
--
homeRoute :: ByteString -> Response
homeRoute bs =
  responseLBS status200 [(hContentType, "application/json")] (fromStrict bs)


-- | Page not found
--
--
notFoundRoute :: Response
notFoundRoute =
  responseLBS status200 [(hContentType, "text/plain")] "Page not found."

-- | Post
--
--
post :: Response
post =
  responseLBS status200 [(hContentType, "text/plain")] "POST method"

-- | PUT
--
--
put :: Response
put =
  responseLBS status200 [(hContentType, "text/plain")] "PUT method"

-- | Delete
--
--
delete :: Response
delete =
  responseLBS status200 [(hContentType, "text/plain")] "DELETE method"
