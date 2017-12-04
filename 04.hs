import Data.List

valid :: [String] -> Bool
valid words = length words == length (nub words)

validCount :: (String -> String) -> [[String]] -> Int
validCount f phrases = length $ filter (valid . map f) phrases

parse :: String -> [[String]]
parse = map words . lines

run :: String -> (Int, Int)
run file = (validCount id phrases, validCount sort phrases)
    where phrases = parse file

main = interact (show . run)
