import qualified Data.Map as Map

data Action = Action
  { writeValue :: Int
  , movePointer :: Int
  , nextState :: String
  }

type Machine = Map.Map (String, Int) Action

type Tape = Map.Map Int Int

data ParseState = ParseState
  { initial :: String
  , steps :: Int
  , current :: String
  , value :: Int
  , machine :: Machine
  }

execute :: String -> Int -> Int -> Tape -> Machine -> Tape
execute _ 0 _ tape _ = tape
execute state steps pointer tape machine =
  execute
    nextState
    (steps - 1)
    (pointer + movePointer)
    (Map.insert pointer writeValue tape)
    machine
  where
    Action { nextState = nextState
           , movePointer = movePointer
           , writeValue = writeValue
           } = machine Map.! (state, valueAt pointer tape)
    valueAt = Map.findWithDefault 0

run :: ParseState -> Int
run ps =
  length $
  filter (== 1) $
  Map.elems $ execute (initial ps) (steps ps) 0 Map.empty (machine ps)

parse :: ParseState -> [String] -> ParseState
parse ps [] = ps
parse ps ["Begin", _, _, s] = ps {initial = s}
parse ps ["Perform", _, _, _, _, n, _] = ps {steps = read n}
parse ps ["In", "state", s] = ps {current = s}
parse ps ["If", _, _, _, _, n] = ps {value = read n}
parse ps ["Write", _, _, n] = ps {machine = machine'}
  where
    machine' = Map.insert key action (machine ps)
    key = (current ps, value ps)
    action = Action (read n) 0 ""
parse ps ["Move", _, _, _, _, s] = ps {machine = machine'}
  where
    machine' = Map.adjust updater key (machine ps)
    key = (current ps, value ps)
    updater action = action {movePointer = delta s}
    delta "left" = -1
    delta "right" = 1
parse ps ["Continue", _, _, s] = ps {machine = machine'}
  where
    machine' = Map.adjust updater key (machine ps)
    key = (current ps, value ps)
    updater action = action {nextState = s}

stripUnnecessary '.' = ' '
stripUnnecessary ':' = ' '
stripUnnecessary '-' = ' '
stripUnnecessary c = c

main =
  interact $
  show .
  run .
  foldl parse (ParseState "" 0 "" 0 Map.empty) .
  map words . lines . map stripUnnecessary
