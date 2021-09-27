{-# LANGUAGE OverloadedStrings #-}

module Lib
    where

import           Data.Aeson
import           Data.Text                 (Text)
import           Network.HTTP.Types        (status200)
import           Network.HTTP.Types.Header (hContentType)
import           Network.Wai
    ( Application
    , Response
    , pathInfo
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
app req f = f $
    case pathInfo req of
        -- Place custom routes here
        _   -> anyRoute

-- | JSON Response
anyRoute :: Response
anyRoute =
    responseLBS
        status200
        [(hContentType, "application/json")]
        (encode jsonData)



-- | JSON data
jsonData = ["a", "b", "c"] :: [Text]
