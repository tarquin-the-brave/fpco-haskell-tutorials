#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import Lens.Micro.Platform
import Test.Hspec

main :: IO ()
main = hspec $
  it "fun with tuples" $
    let tupleLens = _2 . _1
        tuple :: ((Int, Double), (Bool, Char, String))
        tuple = ((1, 2), (True, 'x', "Hello World"))
     in over tupleLens not tuple `shouldBe`
            ((1, 2), (False, 'x', "Hello World"))
