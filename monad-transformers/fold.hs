#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
-- Create a terminating, monadic fold, which allows you to perform effects
-- while stepping through the list. There are many different ways to do this,
-- both with and without monad transformers.
{-# LANGUAGE BangPatterns #-}
foldTerminateM :: Monad m => (b -> a -> m (Either b b)) -> b -> [a] -> m b
foldTerminateM f =
    go
  where
    go !accum rest =
      case rest of
        -- return is in monad m
        [] -> return $ accum
        -- do block is in monad m
        x:xs -> do
          resultAccum' <- f accum x
          case resultAccum' of
            Left accum' -> return accum'
            Right accum' -> go accum' xs

loudSumPositive :: [Int] -> IO Int
loudSumPositive =
    foldTerminateM go 0
  where
    go total x
      | x < 0 = do
          putStrLn "Found a negative, stopping"
          return $ Left total
      | otherwise = do
          putStrLn "Non-negative, continuing"
          let total' = total + x
          putStrLn $ "New total: " ++ show total'
          return $ Right total'

main :: IO ()
main = do
  res <- loudSumPositive [1, 2, 3, -1, 5]
  putStrLn $ "Result: " ++ show res
