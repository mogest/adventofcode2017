import qualified Data.Set as Set
import Data.List

valid :: (String -> String) -> [String] -> Bool
valid f words = go Set.empty words
    where
        go _   []                 = True
        go set (unprocessedWord:words)
            | Set.member word set = False
            | otherwise           = go (Set.insert word set) words
            where
                word = f unprocessedWord

validCount :: (String -> String) -> [[String]] -> Int
validCount f phrases = length $ filter (valid f) phrases

parse :: String -> [[String]]
parse = map words . lines

run :: String -> (Int, Int)
run file = (validCount id phrases, validCount sort phrases)
    where phrases = parse file

main = interact (show . run)
