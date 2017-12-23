module Tablet
  ( execute
  , parse
  , step
  , State(..)
  , Instruction(..)
  , Value(..)
  ) where

import qualified Data.Map as Map

type SndHandler = State -> Int -> State
type RcvHandler = State -> String -> State

data Value
  = Immediate { immediate :: Int }
  | Register { register :: String }

data Instruction
  = SND { value :: Value }
  | SET { r :: String
        , value :: Value }
  | ADD { r :: String
        , value :: Value }
  | SUB { r :: String
        , value :: Value }
  | MUL { r :: String
        , value :: Value }
  | MOD { r :: String
        , value :: Value }
  | RCV { r :: String }
  | JGZ { value :: Value
        , offset :: Value }
  | JNZ { value :: Value
        , offset :: Value }

data State = State
  { registers :: Map.Map String Int
  , pc :: Int
  , halt :: Bool
  } deriving (Show)

evaluate :: Value -> State -> Int
evaluate Immediate {immediate = n} state = n
evaluate Register {register = r} state =
  Map.findWithDefault 0 r (registers state)

step :: [Instruction] -> SndHandler -> RcvHandler -> State -> State
step program sndHandler rcvHandler state
  | pc state < 0 || pc state >= length program =
    State (registers state) (pc state) True
  | otherwise =
    case program !! pc state of
      SND {value = value} ->
        sndHandler state $ evaluate value state
      RCV {r = r} ->
        rcvHandler state r
      SET {r = r, value = value} ->
        nextState $ Map.insert r (evaluate value state) $ registers state
      ADD {r = r, value = value} -> updateRegister (+) r value
      SUB {r = r, value = value} -> updateRegister (-) r value
      MUL {r = r, value = value} -> updateRegister (*) r value
      MOD {r = r, value = value} -> updateRegister mod r value
      JGZ {value = value, offset = offset} ->
        if evaluate value state > 0
          then State
                 (registers state)
                 (pc state + evaluate offset state)
                 False
          else noop
      JNZ {value = value, offset = offset} ->
        if evaluate value state /= 0
          then State
                 (registers state)
                 (pc state + evaluate offset state)
                 False
          else noop
  where
    updateRegister f r value =
      nextState $
      Map.insert r (f (rv r) (evaluate value state)) $ registers state
    rv r = Map.findWithDefault 0 r (registers state)
    noop = nextState (registers state)
    nextState r = State r nextPC False
    nextPC = 1 + pc state

parseValue value =
  case reads value :: [(Int, String)] of
    [(n, "")] -> Immediate n
    _ -> Register value

parseLine ["snd", x] = SND (parseValue x)
parseLine ["set", x, y] = SET x (parseValue y)
parseLine ["add", x, y] = ADD x (parseValue y)
parseLine ["sub", x, y] = SUB x (parseValue y)
parseLine ["mul", x, y] = MUL x (parseValue y)
parseLine ["mod", x, y] = MOD x (parseValue y)
parseLine ["rcv", x] = RCV x
parseLine ["jgz", x, y] = JGZ (parseValue x) (parseValue y)
parseLine ["jnz", x, y] = JNZ (parseValue x) (parseValue y)

execute :: [Instruction] -> SndHandler -> RcvHandler -> State -> [State]
execute program sndHandler rcvHandler = iterate (step program sndHandler rcvHandler)

parse :: String -> [Instruction]
parse = map (parseLine . words) . lines
