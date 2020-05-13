#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE NoImplicitPrelude #-}
import RIO
import RIO.Time (getCurrentTime)
import System.IO (hPutStrLn, stderr, stdout)

data App = App
  { appName :: !String
  , appHandle :: !Handle
  }

data App2 = App2
  { app2FavoriteColor :: !String
  , app2Handle :: !Handle
  }

class HasHandle env where
  getHandle :: env -> Handle
instance HasHandle Handle where
  getHandle = id
instance HasHandle App where
  getHandle = appHandle
instance HasHandle App2 where
  getHandle = app2Handle

class HasFavoriteColor env where
  getFavoriteColor :: env -> String
instance HasFavoriteColor App2 where
  getFavoriteColor = app2FavoriteColor

main :: IO ()
main = do
  let app = App
        { appName = "Alice"
        , appHandle = stderr
        }
  runRIO app $ do
    sayHello
    sayTime
    sayGoodbye
  -- Also works!
  runRIO stdout sayTime

  let app2 = App2
        { app2Handle = stdout
        , app2FavoriteColor = "red"
        }
  runRIO app2 $ do
    sayTime
    sayFavoriteColor

say :: HasHandle env => String -> RIO env ()
say msg = do
  env <- ask
  liftIO $ hPutStrLn (getHandle env) msg

sayTime :: HasHandle env => RIO env ()
sayTime = do
  now <- getCurrentTime
  say $ "The time is: " ++ show now

sayFavoriteColor ::
  HasFavoriteColor env =>
  HasHandle env =>
  RIO env ()
sayFavoriteColor = do
  color <- fmap getFavoriteColor ask
  say $ "Favorite color: " ++ color

sayHello :: RIO App ()
sayHello = do
  App name _h <- ask
  say $ "Hello, " ++ name

sayGoodbye :: RIO App ()
sayGoodbye = do
  App name _h <- ask
  say $ "Goodbye, " ++ name
