#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE NoImplicitPrelude #-}
import RIO
import System.IO (hPutStrLn, stderr, stdout)

data App = App
  { appName :: !String
  , appHandle :: !Handle
  }

class HasHandle env where
  handleL :: Lens' env Handle
instance HasHandle App where
  handleL = lens appHandle (\x y -> x { appHandle = y })

class HasName env where
  nameL :: Lens' env String
instance HasName App where
  nameL = lens appName (\x y -> x { appName = y })

main :: IO ()
main = do
  let app = App
        { appName = "Alice"
        , appHandle = stderr
        }
  runRIO app $ addLastName sayHello

addLastName :: HasName env => RIO env a -> RIO env a
addLastName inner = do
  fullName <- fmap (++ " Smith") $ view nameL
  env' <- fmap (set nameL fullName) ask
  runRIO env' inner

addLastName' :: HasName env => RIO env a -> RIO env a
addLastName' = local $ over nameL (++ " Smith")

say :: HasHandle env => String -> RIO env ()
say msg = do
  h <- view handleL
  liftIO $ hPutStrLn h msg

sayHello :: RIO App ()
sayHello = do
  App name _h <- ask
  say $ "Hello, " ++ name
