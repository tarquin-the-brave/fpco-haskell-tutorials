#!/usr/bin/env stack
-- stack --resolver lts-7.14 --install-ghc runghc
import Text.Read (readMaybe)
import Control.Applicative ((<$>), (<*>))

displayAge maybeAge =
    case maybeAge of
        Nothing -> putStrLn "You provided invalid input"
        Just age -> putStrLn $ "In that year, you will be: " ++ show age

yearDiff futureYear birthYear = futureYear - birthYear
yourHelperFunction f x y
    | x > y = f x y
    | otherwise = f y x

main
    | yourHelperFunction yearDiff 5 6 == 1 = putStrLn "Correct!"
    | otherwise = putStrLn "Please try again"
