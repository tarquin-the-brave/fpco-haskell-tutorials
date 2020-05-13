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

class HasHandle env where
  getHandle :: env -> Handle
instance HasHandle Handle where
  getHandle = id
instance HasHandle App where
  getHandle = appHandle

class SetHandle env where
  setHandle :: Handle -> env -> env
instance SetHandle App where
  setHandle h app = app { appHandle = h }

main :: IO ()
main = do
  let app = App
        { appName = "Alice"
        , appHandle = stderr
        }
  runRIO app $ do
    switchHandle stdout sayHello
    sayTime

switchHandle
  :: SetHandle env
  => Handle
  -> RIO env a
  -> RIO env a
switchHandle h inner = do
  env <- fmap (setHandle h) ask
  runRIO env $ do
    inner

say :: HasHandle env => String -> RIO env ()
say msg = do
  env <- ask
  liftIO $ hPutStrLn (getHandle env) msg

sayTime :: HasHandle env => RIO env ()
sayTime = do
  now <- getCurrentTime
  say $ "The time is: " ++ show now

sayHello :: RIO App ()
sayHello = do
  App name _h <- ask
  say $ "Hello, " ++ name
