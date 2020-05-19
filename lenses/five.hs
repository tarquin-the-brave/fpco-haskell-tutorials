#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import Lens.Micro.Platform
import Test.Hspec

main :: IO ()
main = hspec $ do
  it "over left on left" $
    let val :: Either Int Double
        val = Left 5
     in over _Left (+ 1) val `shouldBe` Left 6
  it "over left on right" $
    let val :: Either Int Double
        val = Right 5
     in over _Left (+ 1) val `shouldBe` Right 5
  it "set left on left" $
    let val :: Either Int Double
        val = Left 5
     in set _Left 6 val `shouldBe` Left 6
  it "set left on right" $
    let val :: Either Int Double
        val = Right 5
     in set _Left 6 val `shouldBe` Right 5
