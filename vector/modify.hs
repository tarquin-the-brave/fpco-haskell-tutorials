#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
-- import Data.Vector.Unboxed (modify)
import Control.Monad.ST

-- TODO: debug this...
modify :: VM.Unbox a => (VM.MVector s a -> ST s ()) -> V.Vector a -> V.Vector a
modify sa v = runST $ do
  mv <- V.thaw v
  _ <- sa mv
  V.freeze mv

main :: IO ()
main = do
  let vec = V.enumFromTo 1 10 :: V.Vector Int
  print $ modify (\v -> fmap (\_-> ()) $ VM.nextPermutation v) vec
