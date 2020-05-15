-- run me with:
-- stack --resolver lts-12.21 ghc --package conduit-combinators -- streamBang.hs -O2 && ./streamBang +RTS -s
{-# LANGUAGE BangPatterns #-}
import Conduit

average :: Monad m => ConduitM Int o m Double
average =
  divide <$> foldlC add (0, 0)
  where
    divide !(total, count) = fromIntegral total / count
    add !(total, count) !x = (total + x, count + 1)

main :: IO ()
main = print $ runConduitPure $ enumFromToC 1 1000000 .| average
