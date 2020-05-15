#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import           Data.Vector.Unboxed         (unsafeFreeze)
import qualified Data.Vector.Unboxed.Mutable as V

main :: IO ()
main = do
    vector <- V.replicate 1 (0 :: Int)
    V.write vector 0 1
    ivector <- unsafeFreeze vector
    print ivector
    V.write vector 0 2
    print ivector
