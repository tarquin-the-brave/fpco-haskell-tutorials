#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE BangPatterns #-}
import Control.DeepSeq

(*$!!) :: NFData a => (a -> b) -> a -> b
infixl 9 *$!!
x *$!! y = y `deepseq` x $ y

average :: [Int] -> Double
average list0 =
  go list0 (0, 0)
  where
    go [] (total, count) = fromIntegral total / count
    go (x:xs) (total, count) = go xs *$!! (total + x, count + 1)

main = print $ average [1..1000000]
