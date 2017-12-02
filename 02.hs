minmaxDifference :: [Int] -> Int
minmaxDifference numbers = maximum numbers - minimum numbers

checksumA :: [[Int]] -> Int
checksumA = sum . map minmaxDifference

--

divisible :: [Int] -> Int
divisible numbers = head [a `div` b | a <- numbers, b <- numbers, a /= b, a `mod` b == 0]

checksumB :: [[Int]] -> Int
checksumB = sum . map divisible

--

parse :: String -> [[Int]]
parse = map (map read . words) . lines

run :: String -> (Int, Int)
run file = (checksumA spreadsheet, checksumB spreadsheet)
    where spreadsheet = parse file

main = interact (show . run)
