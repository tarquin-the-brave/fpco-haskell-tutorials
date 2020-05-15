#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B

main :: IO ()
main = do
  let ip = "source.txt"
  let op = "dest.txt"
  bs <- B.readFile ip
  B.writeFile op bs
