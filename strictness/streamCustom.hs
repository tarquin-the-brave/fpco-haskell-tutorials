-- TODO: Come back to this one...
--
-- run me with:
-- stack --resolver lts-12.21 ghc --package conduit-combinators -- streamCustom.hs -O2 && ./streamCustom +RTS -s
{-# LANGUAGE BangPatterns #-}
import Conduit

data MyData = MyData {
  myTotal :: !Int,
  myCount :: !Int
}

average :: Monad m => ConduitM Int o m Double
average =
  divide <$> foldlC add (MyData 0 0)
  where
    divide (MyData total count) = fromIntegral total / count
    add (MyData total count) x = MyData (total + x) (count + 1)

main :: IO ()
main = print $ runConduitPure $ enumFromToC 1 1000000 .| average
