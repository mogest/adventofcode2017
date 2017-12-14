import KnotHash
import Control.Arrow
import qualified Data.Set as Set

type Disk = Set.Set (Int, Int)

updateConnected :: Disk -> (Int, Int) -> Disk
updateConnected disk position@(x, y) =
  foldl updateConnected (Set.delete position disk) $ filter (flip Set.member disk) neighbours
    where
      neighbours = [(x + dx, y) | dx <- [-1, 1], x + dx >= 0, x + dx <= 127] ++
                   [(x, y + dy) | dy <- [-1, 1], y + dy >= 0, y + dy <= 127]

regionCount :: Disk -> Int
regionCount disk = fst $ foldl search (0, disk) disk
  where
    search (count, disk) position = if Set.member position disk then
      (count + 1, updateConnected disk position)
    else
      (count, disk)

blockCount :: Disk -> Int
blockCount = length

buildDisk :: String -> Disk
buildDisk key = Set.fromList $ concat $ map (build . (id &&& hash)) [0 .. 127]
  where
    hash number          = map (== '1') $ toBinary $ knotHash $ key ++ "-" ++ (show number)
    build (row, columns) = map (cell row) $ filter snd $ zip [0 ..] columns
    cell row (column, _) = (row, column)

run :: String -> (Int, Int)
run key = (blockCount disk, regionCount disk)
  where
    disk = buildDisk key

main = interact (show . run . head . lines)
