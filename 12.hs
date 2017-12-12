import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

type Connections = Map.Map Int [Int]

connected :: Int -> Connections -> Set.Set Int
connected start connections = go Set.empty start
  where
    go visited pipe = foldl go newVisited unvisited
      where
        newVisited = Set.insert pipe visited
        unvisited = filter (not . flip Set.member visited) (connections Map.! pipe)

totalGroups :: Connections -> Int
totalGroups connections = length $ go []
  where
    pipes = Map.keys connections
    go visited
      | null unvisited = visited
      | otherwise      = go $ (connected (head unvisited) connections):visited
      where
        unvisited = pipes \\ (Set.toList $ Set.unions visited)

run :: Connections -> (Int, Int)
run connections = (length $ connected 0 connections, totalGroups connections)

parse :: Connections -> [String] -> Connections
parse connections (a:"<->":bs) = Map.insert (read a) (map read bs) connections

main = interact (show . run . foldl parse Map.empty . map words . lines . decomma)
  where
    decomma = filter (/= ',')
