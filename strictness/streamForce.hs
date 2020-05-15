-- run me with:
-- stack --resolver lts-12.21 ghc --package conduit-combinators -- streamForce.hs -O2 && ./streamForce +RTS -s
import Conduit
import Control.DeepSeq (force)

average :: Monad m => ConduitM Int o m Double
average =
  divide <$> foldlC add (0, 0)
  where
    divide (total, count) = fromIntegral total / count
    add (total, count) x = force (total + x, count + 1)

main :: IO ()
main = print $ runConduitPure $ enumFromToC 1 1000000 .| average
