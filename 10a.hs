numberCount = 256

spin :: Int -> [Int] -> [Int]
spin count numbers = rest ++ current
  where
    (current, rest) = splitAt (count `mod` numberCount) numbers

twist :: [Int] -> (Int, Int) -> [Int]
twist numbers (count, skipCount) = spin skipCount (rest ++ reverse current)
  where
    (current, rest) = splitAt (count `mod` numberCount) numbers

execute :: [Int] -> [Int]
execute lengths = foldl twist [0..numberCount - 1] (zip lengths [0..])

run :: [Int] -> Int
run lengths = product $ take 2 final
  where
    final = spin index $ execute lengths
    index = (length lengths) - ((sum lengths + skipTotal) `mod` numberCount)
    skipTotal = (length lengths * (length lengths + 1)) `div` 2

decomma ',' = ' '
decomma c   = c

main = interact (show . run . map read . words . map decomma)
