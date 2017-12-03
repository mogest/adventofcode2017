import Data.Maybe
import qualified Data.Map as Map

location :: Int -> (Int, Int)
location number = (x, y)
    where
        layer = floor $ ((sqrt (fromIntegral number - 1)) + 1) / 2
        dimension = layer * 2 + 1
        layerFirst = (dimension - 2) ^ 2 + 1
        layerPosition = number - layerFirst
        sideLength = dimension - 1
        sideMidPoint = sideLength `div` 2 - 1
        (side, sidePosition) = layerPosition `divMod` sideLength
        sideDistance = sideMidPoint - sidePosition
        x = case side of 0 -> layer
                         1 -> sideDistance
                         2 -> -layer
                         3 -> -sideDistance
        y = case side of 0 -> -sideDistance
                         1 -> layer
                         2 -> sideDistance
                         3 -> -layer

distance :: Int -> Int
distance number = abs x + abs y
    where (x, y) = location number

accumulating number grid target =
    if result > target then
        result
    else
        let newGrid = Map.insert (x, y) result grid
        in accumulating (number + 1) newGrid target
    where
        (x, y) = location number
        result = sum $ mapMaybe id [Map.lookup (x + dx, y + dy) grid | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

run input = (distance number, accumulating 2 (Map.fromList [((0,0), 1)]) number)
    where number = read input

main = interact (show . run)
