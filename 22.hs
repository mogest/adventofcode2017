import qualified Data.Map as Map

data Cell
  = Weakened
  | Infected
  | Flagged

type Cells = Map.Map (Int, Int) Cell

data Direction
  = N
  | E
  | S
  | W

data State = State
  { cells :: Cells
  , position :: (Int, Int)
  , direction :: Direction
  , infections :: Int
  }

dx N = 0
dx E = 1
dx S = 0
dx W = -1

dy N = -1
dy E = 0
dy S = 1
dy W = 0

turnLeft N = W
turnLeft W = S
turnLeft S = E
turnLeft E = N

turnRight = turnLeft . turnLeft . turnLeft

turnAround = turnLeft . turnLeft

move :: State -> State
move State {cells = c, position = (x, y), direction = d, infections = i} =
  State c newPosition d i
  where
    newPosition = (x + dx d, y + dy d)

burst :: State -> State
burst State {cells = c, position = p, direction = d, infections = i} =
  move $
  case Map.lookup p c of
    Nothing -> State (Map.insert p Weakened c) p (turnLeft d) i
    Just Weakened -> State (Map.insert p Infected c) p d (i + 1)
    Just Infected -> State (Map.insert p Flagged c) p (turnRight d) i
    Just Flagged -> State (Map.delete p c) p (turnAround d) i

run :: Cells -> Int
run cells = infections $ iterate burst initialState !! 10000000
  where
    initialState = State cells (0, 0) N 0

parse :: [String] -> Cells
parse lines = Map.fromList $ concatMap parse' $ zip [start ..] lines
  where
    parse' (y, line) =
      map (parse'' y . fst) $ filter infected $ zip [start ..] line
    parse'' y x = ((x, y), Infected)
    infected (_, char) = char == '#'
    start = -(length lines `div` 2)

main = interact $ show . run . parse . lines
