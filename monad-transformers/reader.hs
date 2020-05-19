#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
-- Define a monad transformer ReaderT, such that the following works:
{-# LANGUAGE DeriveFunctor #-}
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Functor.Identity

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a } deriving Functor

instance Monad m => Applicative (ReaderT r m) where
  pure x = ReaderT $ \_ -> pure x
  ReaderT ff <*> ReaderT fa = ReaderT $ \r -> ff r <*> fa r

instance Monad m => Monad (ReaderT r m) where
  return = pure
  ReaderT f >>= g = ReaderT $ \r -> f r >>= flip runReaderT r . g

instance MonadTrans (ReaderT r) where
  lift action = ReaderT $ \_ -> action

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

type Reader r = ReaderT r Identity

runReader :: Reader r a -> r -> a
runReader r = runIdentity . runReaderT r

ask :: Monad m => ReaderT r m r
ask = ReaderT pure

main :: IO ()
main = runReaderT main' "Hello World"

main' :: ReaderT String IO ()
main' = do
  lift $ putStrLn "I'm going to tell you a message"
  liftIO $ putStrLn "The message is:"
  message <- ask
  lift $ putStrLn message
