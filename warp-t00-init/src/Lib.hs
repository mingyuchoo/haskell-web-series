{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import           Network.HTTP.Types        (status200)
import           Network.HTTP.Types.Header (hContentType)
import           Network.Wai               (Application, responseLBS)
import           Network.Wai.Handler.Warp  (run)

-- |
someFunc :: IO ()
someFunc = do
    let port = 3000
    putStrLn $ "listening on " ++ show port
    run port (\req f ->
        f $ responseLBS
                status200
                [(hContentType, "text/plain")]
                "Hello, World!\n")

