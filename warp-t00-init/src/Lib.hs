{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Lib
    ( someFunc
    ) where

import           Data.Kind                 ()
import           Network.HTTP.Types        (status200)
import           Network.HTTP.Types.Header (hContentType)
import           Network.Wai               (Response, responseLBS)
import           Network.Wai.Handler.Warp  (run)

-- |
--
--
someFunc :: IO ()
someFunc = do
    let port = 3000
    putStrLn $ "listening on " ++ show port
    run port app

app :: p -> (Response -> t) -> t
app req f = f $ responseLBS
                    status200
                    [(hContentType, "text/plain")]
                    "Hello, World!\n"
