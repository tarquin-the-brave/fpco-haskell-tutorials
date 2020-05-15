#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import           Control.Monad               (replicateM_)
import           Data.Vector.Unboxed         (unsafeFreeze)
import qualified Data.Vector.Unboxed.Mutable as V
import           System.Random               (randomRIO)

main :: IO ()
main = do
    vector <- V.replicate 10 (0 :: Int)

    replicateM_ (10^6) $ do
        i <- randomRIO (0, 9)
        oldCount <- V.unsafeRead vector i
        V.unsafeWrite vector i (oldCount + 1)

    ivector <- unsafeFreeze vector
    print ivector
