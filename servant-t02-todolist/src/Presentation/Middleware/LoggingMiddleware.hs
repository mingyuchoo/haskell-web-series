-- | Middleware for logging HTTP requests and responses
module Presentation.Middleware.LoggingMiddleware
    ( -- * Middleware
      loggingMiddleware
    ) where

-- -------------------------------------------------------------------
-- Imports
-- -------------------------------------------------------------------

import           Control.Exception      (SomeException, try)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import           Data.ByteString.Char8  (unpack)
import qualified Data.ByteString.Lazy   as LBS
import           Data.IORef             (IORef, modifyIORef, newIORef, readIORef)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Data.Time              (UTCTime, getCurrentTime)
import           Flow                   ((<|))
import           Network.HTTP.Types     (Method, Status, statusCode)
import           Network.Wai
    ( Middleware
    , Request
    , Response
    , ResponseReceived
    , pathInfo
    , requestBody
    , requestHeaders
    , requestMethod
    , responseHeaders
    , responseStatus
    )
import qualified Network.Wai            as Wai (rawQueryString)
import           System.IO              (hFlush, stdout)

-- -------------------------------------------------------------------
-- Middleware Implementation
-- -------------------------------------------------------------------

-- | Middleware that logs HTTP requests and responses to the terminal
loggingMiddleware :: Middleware
loggingMiddleware app req sendResponse = do
    -- Log the request
    time <- getCurrentTime
    logSection "REQUEST" time
    logRequestInfo req
    
    -- Capture the request body
    (req', bodyContent) <- captureRequestBody req
    
    -- Log the body content (safely)
    logBodyContent bodyContent
    hFlush stdout  -- Ensure output is displayed immediately
    
    -- Call the application with the modified request and intercept the response
    app req' <| \res -> do
        -- Log the response
        logSection "RESPONSE" time
        logResponseInfo res
        hFlush stdout  -- Ensure output is displayed immediately

        -- Send the response to the client
        sendResponse res

-- | Log a section header with timestamp
logSection :: String -> UTCTime -> IO ()
logSection name time = putStrLn <| "\n[" ++ name ++ "] " ++ show time

-- | Log basic request information
logRequestInfo :: Request -> IO ()
logRequestInfo req = do
    putStrLn <| "  Method: " ++ unpack (requestMethod req)
    putStrLn <| "  Path: /" ++ showPath (pathInfo req)
    putStrLn <| "  Headers: " ++ show (requestHeaders req)
    putStrLn <| "  Query Parameters: " ++ unpack (Wai.rawQueryString req)
  where
    showPath [] = ""
    showPath xs = unwords (map show xs)

-- | Log response information
logResponseInfo :: Response -> IO ()
logResponseInfo res = do
    putStrLn <| "  Status: " ++ show (statusCode <| responseStatus res)
    putStrLn <| "  Headers: " ++ show (responseHeaders res)

-- | Log body content safely
logBodyContent :: ByteString -> IO ()
logBodyContent body = do
    putStrLn <| "  Body Length: " ++ show (BS.length body) ++ " bytes"
    if BS.null body
        then putStrLn <| "  Body: <empty>"
        else do
            -- Try to decode as UTF-8 text, fallback to showing as binary if it fails
            result <- try (evaluate (TE.decodeUtf8 body)) :: IO (Either SomeException Text)
            case result of
                Right text -> putStrLn <| "  Body: " ++ T.unpack text
                Left _     -> putStrLn <| "  Body: <binary data>"
  where
    evaluate :: a -> IO a
    evaluate a = return a

-- | Capture the request body and create a new request with the body restored
captureRequestBody :: Request -> IO (Request, ByteString)
captureRequestBody req = do
    -- Read all body chunks
    bodyChunks <- readRequestBodyChunks req
    
    -- Convert chunks to a single ByteString
    let bodyContent = BS.concat bodyChunks
    
    -- Create a reference to store the body for later use
    bodyRef <- newIORef [bodyContent]
    
    -- Create a new request with the body restored
    let req' = req { requestBody = getBodyChunk bodyRef }
    
    return (req', bodyContent)

-- | Read all chunks from the request body
readRequestBodyChunks :: Request -> IO [ByteString]
readRequestBodyChunks req = do
    chunk <- requestBody req
    if BS.null chunk
        then return []
        else do
            chunks <- readRequestBodyChunks req
            return (chunk : chunks)

-- | Create a requestBody function that returns chunks from our stored body
getBodyChunk :: IORef [ByteString] -> IO ByteString
getBodyChunk ref = do
    chunks <- readIORef ref
    case chunks of
        []     -> return BS.empty
        (x:xs) -> do
            modifyIORef ref (const xs)
            return x
