import KnotHash
import Control.Arrow
import qualified Data.Map as Map

data Cell = Free | Used | Scanned deriving Eq

type Disk = Map.Map (Int, Int) Cell

updateConnected :: Disk -> (Int, Int) -> Disk
updateConnected disk position@(x, y) = case disk Map.! position of
  Used -> foldl updateConnected (Map.insert position Scanned disk) neighbours
  _    -> disk
  where
    neighbours = [(x + dx, y) | dx <- [-1, 1], x + dx >= 0, x + dx <= 127] ++
                 [(x, y + dy) | dy <- [-1, 1], y + dy >= 0, y + dy <= 127]

regionCount :: Disk -> Int
regionCount disk = fst $ foldl search (0, disk) $ [(x, y) | x <- [0 .. 127], y <- [0 .. 127]]
  where
    search (count, disk) position = case disk Map.! position of
      Used -> (count + 1, updateConnected disk position)
      _    -> (count, disk)

blockCount :: Disk -> Int
blockCount disk = length $ filter (== Used) $ map snd $ Map.toList disk

buildDisk :: String -> Disk
buildDisk key = foldl build Map.empty $ map (id &&& hash) [0 .. 127]
  where
    hash number = map toCell $ toBinary $ knotHash $ key ++ "-" ++ (show number)
    toCell '0' = Free
    toCell '1' = Used
    build disk (row, columns) = foldl (buildRow row) disk $ zip [0 ..] columns
    buildRow row disk (column, value) = Map.insert (row, column) value disk

run :: String -> (Int, Int)
run key = (blockCount disk, regionCount disk)
  where
    disk = buildDisk key

main = interact (show . run . head . lines)
