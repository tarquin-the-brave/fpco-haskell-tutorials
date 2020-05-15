-- run me with:
-- stack --resolver lts-12.21 ghc chain.hs
-- ./chain +RTS -s
{-# LANGUAGE BangPatterns #-}

data StrictList a = Cons !a !(StrictList a) | Nil

strictMap :: (a -> b) -> StrictList a -> StrictList b
strictMap _ Nil = Nil
strictMap f (Cons a list) =
  let !b = f a
      !list' = strictMap f list
   in b `seq` list' `seq` Cons b list'

strictEnum :: Int -> Int -> StrictList Int
strictEnum low high =
  go low
  where
    go !x
      | x == high = Cons x Nil
      | otherwise = Cons x (go $! x + 1)

double :: Int -> Int
double !x = x * 2

evens :: StrictList Int
evens = strictMap double $! strictEnum 1 1000000

main :: IO ()
main = do
  let string = "Hello World"
      -- !string' = evens `seq` string
      string' = evens `seq` string
  -- putStrLn string
  putStrLn $ string' `seq` string
