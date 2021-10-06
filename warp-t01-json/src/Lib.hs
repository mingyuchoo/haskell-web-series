{-# LANGUAGE OverloadedStrings #-}

module Lib
    where

import           Data.Aeson
import           Data.ByteString           (ByteString, length)
import           Data.ByteString.Lazy      (fromStrict)
import           Data.Text                 (Text, pack)
import           Network.HTTP.Types        (status200)
import           Network.HTTP.Types.Header (hContentType)
import           Network.HTTP.Types.URI    (Query)
import           Network.Wai
    ( Application
    , Response
    , pathInfo
    , queryString
    , responseLBS
    )
import           Network.Wai.Handler.Warp  (run)

-- | Main Function
someFunc :: IO ()
someFunc = do
    let port = 3000
    putStrLn $ "listening on " ++ show port
    run port app

-- | Application
app :: Application
app req res = do
    case (pathInfo req, queryString req) of
      (["expr"], [("q", Just stuff)]) -> res $ homeRoute stuff
      _                               -> res $ notFoundRoute

-- | JSON Response
homeRoute :: ByteString -> Response
homeRoute bs =
    responseLBS
        status200
        [(hContentType, "application/json")]
        (fromStrict bs)

-- | Page not found
notFoundRoute :: Response
notFoundRoute =
    responseLBS
        status200
        [(hContentType, "text/plain")]
        "Page not found."

-- | JSON data
jsonData :: [Text]
jsonData = (pack . show) <$> [1 .. 100]

