import Data.Bits

aMultiplier = 16807
bMultiplier = 48271

generator1 :: Int -> Int -> [Int]
generator1 seed multiplier = iterate next (next seed)
  where
    next value = (value * multiplier) `mod` 2147483647

generator2 :: Int -> Int -> Int -> [Int]
generator2 guard seed multiplier = filter criterion $ generator1 seed multiplier
  where
    criterion value = value .&. guard == 0

countMatches :: (Int, Int) -> (Int -> Int -> [Int], Int -> Int -> [Int]) -> Int -> Int
countMatches (aSeed, bSeed) (generatorA, generatorB) runs =
  length $ filter judge $ take runs $ generators
    where
      generators = zip (generatorA aSeed aMultiplier) (generatorB bSeed bMultiplier)
      judge (a, b) = (a .&. 65535) == (b .&. 65535)

run :: (Int, Int) -> (Int, Int)
run seeds = ( countMatches seeds (generator1, generator1) 40000000
            , countMatches seeds (generator2 3, generator2 7) 5000000
            )

parse :: [String] -> (Int, Int)
parse (aSeed:bSeed:[]) = (read aSeed, read bSeed)

main = interact (show . run . parse . words)
