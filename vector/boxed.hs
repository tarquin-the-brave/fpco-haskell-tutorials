#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import qualified Data.Vector as V

main :: IO ()
main = do
  -- from tutorial
  let list = [1..10] :: [Int]
      vector = V.fromList list :: V.Vector Int
      vector2 = V.enumFromTo 1 10 :: V.Vector Int
  print $ vector == vector2 -- True
  print $ list == V.toList vector -- also True
  print $ V.filter odd vector -- 1,3,5,7,9
  print $ V.map (* 2) vector -- 2,4,6,...,20
  print $ V.zip vector vector -- (1,1),(2,2),...(10,10)
  print $ V.zipWith (*) vector vector -- (1,4,9,16,...,100)
  print $ V.reverse vector -- 10,9,...,1
  print $ V.takeWhile (< 6) vector -- 1,2,3,4,5
  print $ V.takeWhile odd vector -- 1
  print $ V.takeWhile even vector -- []
  print $ V.dropWhile (< 6) vector -- 6,7,8,9,10
  print $ V.head vector -- 1
  print $ V.tail vector -- 2,3,4,...,10

  -- my additions
  let vec = V.enumFromTo 1 10
  print vec
  print $ V.foldl1' (+) vec
  print $ V.elemIndices 5 vec
  print $ V.elemIndices 15 vec
  print $ fmap (10 +) vec
  print $ 'f' <$ vec
  print $ 0 <$ vec
  print $ foldr (-) 0 vec
  print $ foldl (-) 0 vec
  print $ mapM Just vec
