#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import Lens.Micro.Platform
import Data.Text (Text)
import Test.Hspec

data Address = Address
  { _street :: !Text
  , _city :: !Text
  }

city :: Lens' Address Text
city = \f s -> (\x y -> x { _city = y }) s <$> f (_city s)

street :: Lens' Address Text
street = \f s -> (\x y -> x { _street = y }) s  <$> f (_street s)

data Person = Person
  { _name :: !Text
  , _address :: !Address
  , _age :: !Int
  }

name :: Lens' Person Text
name = \f s -> (\x y -> x { _name = y }) s <$> f (_name s)

address :: Lens' Person Address
address = \f s -> (\x y -> x { _address = y }) s <$> f (_address s)

age :: Lens' Person Int
age = \f s -> (\x y -> x { _age = y }) s <$> f (_age s)


hollywood :: Text
hollywood = "Hollywood Blvd"

alice :: Person
alice = Person
  { _name = "Alice"
  , _address = Address
      { _street = hollywood
      , _city = "Los Angeles"
      }
  , _age = 30
  }

wilshire :: Text
wilshire = "Wilshire Blvd"

aliceWilshire :: Person
aliceWilshire = address . street %~ (\_ -> wilshire) $ alice

getStreet :: Person -> Text
getStreet = view $ address . street

-- | Increase age by 1
birthday :: Person -> Person
birthday = over age (1+)
-- birthday = age `over` (1+)
-- birthday = age %~ (1+)
-- birthday = (%~) age (1+)

getAge :: Person -> Int
getAge = flip (^.) age
-- getAge = view age
-- getAge p = p ^. age

main :: IO ()
main = hspec $ do
  it "lives on Wilshire" $
    _street (_address aliceWilshire) `shouldBe` wilshire
  it "getStreet works" $
    getStreet alice `shouldBe` hollywood
  it "birthday" $
    getAge (birthday alice) `shouldBe` _age alice + 1
