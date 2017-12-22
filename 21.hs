import Data.List (transpose)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map.Strict as Map (Map, fromList, lookup)
import Data.Maybe (fromJust)

type Image = [[Bool]]

type Rules = Map.Map Image Image

initialImage = ".#./..#/###"

toImage :: String -> Image
toImage s = map (map fromChar) $ splitOn "/" s
  where
    fromChar '.' = False
    fromChar '#' = True

recombine :: [Image] -> Image
recombine pieces = concatMap (map concat . transpose) $ chunksOf size pieces
  where
    size = floor $ sqrt $ fromIntegral $ length pieces

substitute :: Rules -> [Image] -> [Image]
substitute rules = map substitute'
  where
    substitute' :: Image -> Image
    substitute' piece =
      fromJust $ head $ dropWhile (== Nothing) $ map (search piece) rotations
    search piece f = Map.lookup (f piece) rules
    flipH = map reverse
    rotateL = transpose . flipH
    rotations =
      [ id
      , rotateL
      , rotateL . rotateL
      , rotateL . rotateL . rotateL
      , flipH
      , flipH . rotateL
      , flipH . rotateL . rotateL
      , flipH . rotateL . rotateL . rotateL
      ]

breakInto :: Int -> Image -> [Image]
breakInto size image =
  map transpose $ chunksOf size $ concatMap transpose $ chunksOf size image

enhance :: Rules -> Image -> Image
enhance rules image = recombine $ substitute rules $ breakInto size image
  where
    size =
      if length image `mod` 2 == 0
        then 2
        else 3

countPixels :: Image -> Int
countPixels image = length $ filter id $ concat image

art rules iterations =
  countPixels $
  head $ drop iterations $ iterate (enhance rules) $ toImage initialImage

run rules = (art rules 5, art rules 18)

parse [src, "=>", dest] = (toImage src, toImage dest)

main = interact (show . run . Map.fromList . map (parse . words) . lines)
