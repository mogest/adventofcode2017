module KnotHash (knotHash, toHex, toBinary) where

import Data.Bits (xor)
import Text.Printf

numberCount = 256
denseCount = 16
lengthSuffix = [17, 31, 73, 47, 23]

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

dense :: [Int] -> [Int]
dense numbers = map (xor' . take denseCount) $ takeWhile (/= []) $ iterate (drop denseCount) numbers
  where
    xor' = foldl xor 0

toHex :: [Int] -> String
toHex numbers = concat $ map (printf "%02x") numbers

toBinary :: [Int] -> String
toBinary numbers = concat $ map (printf "%08b") numbers

knotHash :: String -> [Int]
knotHash string = dense final
  where
    roundLengths = (map fromEnum string) ++ lengthSuffix
    lengths = concat $ replicate 64 roundLengths
    final = spin index $ execute lengths
    index = (length lengths) - ((sum lengths + skipTotal) `mod` numberCount)
    skipTotal = (length lengths * (length lengths + 1)) `div` 2
