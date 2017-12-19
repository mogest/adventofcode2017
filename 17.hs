import Data.List

spin step values nextValue = nextValue:b ++ a
  where
    at = step `mod` (length values)
    (a, b) = splitAt (at + 1) values

partA step = (foldl (spin step) [0] [1 .. 2017]) !! 1

fake step (position, afterZero) value
  | position' == 1  = (position', value)
  | otherwise       = (position', afterZero)
  where
    position' = ((position + step) `mod` value) + 1

partB step = snd $ foldl' (fake step) (0, 0) [1 .. 50000000]

run step = (partA step, partB step)

main = interact (show . run . read . head . lines)
