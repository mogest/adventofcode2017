import qualified Data.Map as Map

data Action = Action
  { write :: Int
  , move :: Int
  , next :: String
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

execute :: String -> Int -> Int -> Machine -> Tape -> Tape
execute state 0 pointer machine tape = tape
execute state steps pointer machine tape =
  execute
    (next action)
    (steps - 1)
    (pointer + move action)
    machine
    (Map.insert pointer (write action) tape)
  where
    action = machine Map.! (state, valueAt pointer tape)
    valueAt = Map.findWithDefault 0

run :: ParseState -> Int
run ps =
  length $
  filter (== 1) $
  Map.elems $ execute (initial ps) (steps ps) 0 (machine ps) Map.empty

parse :: ParseState -> [String] -> ParseState
parse ps [] = ps
parse ps ["Begin", _, _, s] = ParseState s 0 "" 0 Map.empty
parse ps ["Perform", _, _, _, _, n, _] =
  ParseState (initial ps) (read n) "" 0 Map.empty
parse ps ["In", "state", s] =
  ParseState (initial ps) (steps ps) s 0 (machine ps)
parse ps ["If", _, _, _, _, "0"] =
  ParseState (initial ps) (steps ps) (current ps) 0 (machine ps)
parse ps ["If", _, _, _, _, "1"] =
  ParseState (initial ps) (steps ps) (current ps) 1 (machine ps)
parse ps ["Write", _, _, n] =
  ParseState (initial ps) (steps ps) (current ps) (value ps) up
  where
    key = (current ps, value ps)
    a = Action (read n) 0 ""
    up = Map.insert key a (machine ps)
parse ps ["Move", _, _, _, _, "left"] =
  ParseState (initial ps) (steps ps) (current ps) (value ps) up
  where
    key = (current ps, value ps)
    up = Map.adjust u key (machine ps)
    u a = Action (write a) (-1) ""
parse ps ["Move", _, _, _, _, "right"] =
  ParseState (initial ps) (steps ps) (current ps) (value ps) up
  where
    key = (current ps, value ps)
    up = Map.adjust u key (machine ps)
    u a = Action (write a) 1 ""
parse ps ["Continue", _, _, s] =
  ParseState (initial ps) (steps ps) (current ps) (value ps) up
  where
    key = (current ps, value ps)
    up = Map.adjust u key (machine ps)
    u a = Action (write a) (move a) s

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
