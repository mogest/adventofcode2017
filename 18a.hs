import qualified Data.Map as Map

data Value = Immediate { immediate :: Int } | Register { register :: String }

data Instruction =
  Snd { value :: Value } |
  Set { r :: String, value :: Value } |
  Add { r :: String, value :: Value } |
  Mul { r :: String, value :: Value } |
  Mod { r :: String, value :: Value } |
  Rcv { value :: Value } |
  Jgz { value :: Value, offset :: Value }

data State = State { registers :: Map.Map String Int
                   , pc :: Int
                   , lastSound :: Int
                   , halt :: Bool
                   } deriving Show

evaluate :: Value -> State -> Int
evaluate Immediate {immediate = n} state = n
evaluate Register {register = r} state = (registers state) Map.! r

execute :: [Instruction] -> State -> State
execute program state
  | pc state < 0 || pc state >= length program = State (registers state) (pc state) (lastSound state) True
  | otherwise = case program !! pc state of
    Snd {value=value} -> State (registers state) nextPC (evaluate value state) False
    Set {r=r, value=value} -> nextState $ Map.insert r (evaluate value state) $ registers state
    Add {r=r, value=value} -> updateRegister (+) r value
    Mul {r=r, value=value} -> updateRegister (*) r value
    Mod {r=r, value=value} -> updateRegister mod r value
    Rcv {value=value} -> if (evaluate value state) /= 0 then State (registers state) (pc state) (lastSound state) True else noop
    Jgz {value=value, offset=offset} -> if (evaluate value state) > 0 then State (registers state) ((pc state) + (evaluate offset state)) (lastSound state) False else noop
  where
    updateRegister f r value = nextState $ Map.insert r (f (rv r) (evaluate value state)) $ registers state
    rv r = Map.findWithDefault 0 r (registers state)
    noop = nextState (registers state)
    nextState r = State r nextPC (lastSound state) False
    nextPC = 1 + pc state

run program = takeWhile (not . halt) $ iterate (execute program) (State Map.empty 0 0 False)

parseValue value =
  case reads value :: [(Int, String)] of
    [(n, "")] -> Immediate n
    _         -> Register value

parse ("snd":x:[])   = Snd (parseValue x)
parse ("set":x:y:[]) = Set x (parseValue y)
parse ("add":x:y:[]) = Add x (parseValue y)
parse ("mul":x:y:[]) = Mul x (parseValue y)
parse ("mod":x:y:[]) = Mod x (parseValue y)
parse ("rcv":x:[])   = Rcv (parseValue x)
parse ("jgz":x:y:[]) = Jgz (parseValue x) (parseValue y)

main = interact (show . run . map (parse . words) . lines)
