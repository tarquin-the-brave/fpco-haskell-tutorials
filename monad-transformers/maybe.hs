#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
-- The implementation of ageInYear below is unpleasant. Use MaybeT to clean
-- it up.
import Control.Monad.Trans.Maybe
import Text.Read (readMaybe)
import System.IO

prompt :: Read a => String -> IO (Maybe a)
prompt question = do
  putStr question
  putStr ": "
  hFlush stdout
  answer <- getLine
  return $ readMaybe answer

ageInYear :: IO (Maybe Int)
ageInYear = do
  runMaybeT $ do
    birthYear <- MaybeT $ prompt "Birth year"
    futureYear <- MaybeT $ prompt "Future year"
    return $ futureYear - birthYear

main :: IO ()
main = do
  mage <- ageInYear
  case mage of
    Nothing -> putStrLn $ "Some problem with input, sorry"
    Just age -> putStrLn $ "In that year, age will be: " ++ show age
