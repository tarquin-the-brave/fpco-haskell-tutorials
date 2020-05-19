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
city = lens _city (\x y -> x { _city = y })

street :: Lens' Address Text
street = lens _street (\x y -> x { _street = y })

data Person = Person
  { _name :: !Text
  , _address :: !Address
  , _age :: !Int
  }

name :: Lens' Person Text
name = lens _name (\x y -> x { _name = y })

address :: Lens' Person Address
address = lens _address (\x y -> x { _address = y })

age :: Lens' Person Int
age = lens _age (\x y -> x { _age = y })


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
