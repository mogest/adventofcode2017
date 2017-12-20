import Data.List
import Data.Maybe

data Direction = N | E | S | W

data State = Running { x :: Int
                     , y :: Int
                     , direction :: Direction
                     , letters :: String
                     }
           | Stopped { letters :: String }

data Cell = Empty | Path | Corner | Letter { letter :: Char } deriving Eq

dx N = 0
dx S = 0
dx W = -1
dx E = 1

dy N = -1
dy S = 1
dy W = 0
dy E = 0

at x y grid
  | y < 0 || y >= length grid  = Empty
  | x < 0 || x >= length row   = Empty
  | otherwise                  = case row !! x of
                                   ' '   -> Empty
                                   '|'   -> Path
                                   '-'   -> Path
                                   '+'   -> Corner
                                   char  -> Letter char
  where
    row = grid !! y

turnCorner grid Running {x=x, y=y, direction=direction, letters=letters} =
  Running newX newY newDir letters
    where
      newDir              = head $ filter cellPresent $ filter oppositeDirection [N, E, S, W]
      oppositeDirection d = dx d /= dx direction && dy d /= dy direction
      cellPresent d       = at (x + dx d) (y + dy d) grid /= Empty
      newX                = x + dx newDir
      newY                = y + dy newDir

move grid state@Running {x=x, y=y, direction=direction, letters=letters} =
  case at x y grid of
    Empty       -> Stopped (reverse letters)
    Path        -> Running (x + dx direction) (y + dy direction) direction letters
    Corner      -> turnCorner grid state
    Letter char -> Running (x + dx direction) (y + dy direction) direction (char:letters)

run grid = (letters $ head finished, length path - 1)
  where
    (path, finished)   = span running $ iterate (move grid) startState
    startState         = Running (fromJust $ findIndex (== '|') (head grid)) 0 S ""
    running Running {} = True
    running Stopped {} = False

main = interact (show . run . lines)
