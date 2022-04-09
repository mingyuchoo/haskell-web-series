{-# LANGUAGE OverloadedStrings #-}

module Main
    where

import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.IORef
import qualified Data.Text               as T
import qualified Database.Persist.Sqlite as Db
import           Network.HTTP.Types
import           Web.Spock
import           Web.Spock.Config

import           System.IO
    ( BufferMode (NoBuffering)
    , hSetBuffering
    , stdout
    )

-- |
--
--
data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

-- |
--
--
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    Db.runSqlite "example.db" $ Db.runMigration migrateAll
    runNoLoggingT $
        Db.withSqlitePool "example.db" 10 $ \pool -> liftIO $ do
        ref <- newIORef 0
        spockCfg <- defaultSpockCfg EmptySession (PCPool pool) (DummyAppState ref)
        runSpock 8000 (spock spockCfg app)

-- |
--
--
app :: SpockM Db.SqlBackend MySession MyAppState ()
app = do
    get root $ text "Hello, World!"
    get ("hello" <//> var) $ \name -> do
        (DummyAppState ref) <- getState
        visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
        text ("Hello, " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))
    get "about" $ html $ mconcat
        [ "<html><body>"
        , "  <h1>Hello Practical Haskell!</h1>"
        , "</body></html"
        ]
    hookAnyAll $ \_route -> do
        setStatus notFound404
        html "<h1>Not Found :(</h1>"



