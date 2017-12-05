import qualified Data.Sequence as S

updaterB :: Int -> Int
updaterB offset
  | offset >= 3 = offset - 1
  | otherwise   = offset + 1

execute :: (Int -> Int) -> [Int] -> Int
execute updater offsets = go (S.fromList offsets) 0 0
  where
    go offsets pointer counter =
      case S.lookup pointer offsets of
        Nothing     -> counter
        Just offset -> go (S.update pointer (updater offset) offsets) (pointer + offset) $! (counter + 1)

run :: [Int] -> (Int, Int)
run offsets = (execute (1 +) offsets, execute updaterB offsets)

main = interact (show . run . map read . lines)
