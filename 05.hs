import qualified Data.Sequence as S

updaterA :: Int -> Int
updaterA offset = offset + 1

updaterB :: Int -> Int
updaterB offset
  | offset >= 3 = offset - 1
  | otherwise   = offset + 1

execute :: (Int -> Int) -> [Int] -> Int
execute updater offsets = go updater (S.fromList offsets) 0 0
  where
    go updater offsets pointer counter =
      case S.lookup pointer offsets of
        Nothing     -> counter
        Just offset -> go updater (S.update pointer (updater offset) offsets) (pointer + offset) (counter + 1)

run :: [Int] -> (Int, Int)
run offsets = (execute updaterA offsets, execute updaterB offsets)

main = interact (show . run . map read . lines)
