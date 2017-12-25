import Control.Arrow (second)
import Data.List (delete, maximumBy)
import qualified Data.Map as Map
import GHC.Exts (groupWith)

type Components = Map.Map Int [Int]

build :: Int -> Int -> Components -> [(Int, Int)]
build left len comps
  | null result = [(len, 0)]
  | otherwise = result
  where
    result = concatMap build' (comps Map.! left)
    build' right =
      map (second (left + right +)) $
      build right (len + 1) (del (left, right) $ del (right, left) comps)
    del (a, b) = Map.adjust (delete b) a

run :: Components -> Int
run comps = snd $ maximumBy longest $ build 0 0 comps
  where
    longest (len1, str1) (len2, str2) = longest' (compare len1 len2) (compare str1 str2)
    longest' EQ x = x
    longest' x _ = x

parse :: String -> (Int, Int)
parse line = (read a, read b)
  where
    [a, b] = words $ map removeSlash line
    removeSlash '/' = ' '
    removeSlash c = c

componentReverse comps = comps ++ map r comps
  where
    r (a, b) = (b, a)

group :: [(Int, Int)] -> [(Int, [Int])]
group comps = map x $ groupWith fst comps
  where
    x c = (fst $ head c, map snd c)

main =
  interact $
  show . run . Map.fromList . group . componentReverse . map parse . lines
