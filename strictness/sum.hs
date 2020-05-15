#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE BangPatterns #-}

(*$!) :: (a -> b) -> a -> b
infixl 9 *$!
x *$! y = y `seq` x $ y


mysum :: [Int] -> Int
mysum list0 =
  go list0 0
  where
    go [] total = total
    go (x:xs) total = go xs *$! total + x

main = print $ mysum [1..1000000]

