#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Lens.Micro.Platform
import Data.Text (Text)
import Test.Hspec

data Address = Address
  { _street :: !Text
  , _city :: !Text
  }

makeLenses ''Address

data Person = Person
  { _name :: !Text
  , _address :: !Address
  , _age :: !Int
  }

makeLenses ''Person

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
