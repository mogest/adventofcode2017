import Data.List
import Data.Tree
import Data.Maybe
import Control.Arrow

data Program = Program { name :: String
                       , weight :: Int
                       , children :: [String]
                       } deriving Show

findUnbalanced :: Int -> Tree Program -> Int
findUnbalanced difference tree =
  case badWeight of
    Nothing     -> (weight $ rootLabel tree) - difference
    Just weight -> findUnbalanced (weight - goodWeight) (badNode weight)
  where
    weigh subtree  = (weight $ rootLabel subtree) + (sum $ map weigh (subForest subtree))
    branches       = map (id &&& weigh) (subForest tree)
    weights        = map snd branches
    count weight   = length $ filter (weight ==) weights
    single weight  = count weight == 1
    badWeight      = find single (nub weights)
    goodWeight     = fromJust $ find (not . single) (nub weights)
    badNode weight = fst $ fromJust $ find ((weight ==) . snd) branches

findRoot :: [Program] -> String
findRoot programs = head $ allNames \\ allChildren
  where
    allNames = map name programs
    allChildren = concat $ map children programs

run :: [Program] -> (String, Int)
run programs = (root, unbalanced)
  where
    root = findRoot programs
    unbalanced = findUnbalanced 0 $ unfoldTree unfolder root
    unfolder root = (program, children program)
      where program = fromJust $ find (\p -> root == name p) programs

parse :: [String] -> Program
parse (name:weight:[]) = parse (name:weight:"->":[])
parse (name:weight:"->":children) = Program name weight' children'
  where
    weight'   = read $ init $ tail weight
    children' = map (delete ',') children

main = interact (show . run . map (parse . words) . lines)
