#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import qualified Data.Vector                 as VB
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as MG
import           System.Random               (StdGen, getStdGen, randomR)

shuffleM :: (PrimMonad m, MG.MVector v a)
         => StdGen
         -> Int -- ^ count to shuffle
         -> v (PrimState m) a
         -> m ()
shuffleM _ i _ | i <= 1 = return ()
shuffleM gen i v = do
    MG.swap v i' index
    shuffleM gen' i' v
  where
    (index, gen') = randomR (0, i') gen
    i' = i - 1

shuffle :: VG.Vector v a
        => StdGen
        -> v a
        -> v a
shuffle gen vector = VG.modify (shuffleM gen (VG.length vector)) vector

main :: IO ()
main = do
    gen <- getStdGen
    print $ shuffle gen $ VU.enumFromTo 1 (20 :: Int)
    print $ shuffle gen $ VB.enumFromTo 1 (20 :: Int)
