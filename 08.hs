import Data.Maybe
import qualified Data.Map as Map

data Command = Command { register :: String
                       , instruction :: String
                       , value :: Int
                       , testRegister :: String
                       , testOperator :: String
                       , testValue :: Int
                       } deriving Show

type Registers = Map.Map String Int

load :: String -> Registers -> Int
load register registers = fromMaybe 0 $ registers Map.!? register

update :: String -> (Int -> Int) -> Registers -> Registers
update register updater registers = Map.insert register (updater $ load register registers) registers

evaluateIf :: Registers -> Command -> Bool
evaluateIf registers command
  | op == ">"  = v1 >  v2
  | op == "<"  = v1 <  v2
  | op == ">=" = v1 >= v2
  | op == "<=" = v1 <= v2
  | op == "==" = v1 == v2
  | op == "!=" = v1 /= v2
    where
      v1 = load (testRegister command) registers
      v2 = testValue command
      op = testOperator command

execute :: Registers -> Command -> Registers
execute registers command
  | evaluateIf registers command = execute'
  | otherwise                    = registers
    where
      ins = instruction command
      reg = register command
      val = value command
      execute'
        | ins == "inc" = update reg (val +) registers
        | ins == "dec" = update reg (flip (-) val) registers

run :: [Command] -> (Int, Int)
run commands = (largestAtEnd, largestAtAnyTime)
  where
    largestAtAnyTime = maximum $ concat history
    largestAtEnd = maximum $ last history
    history = map Map.elems $ scanl execute Map.empty commands

parse :: [String] -> Command
parse (a:b:c:"if":d:e:f:[]) = Command a b (read c) d e (read f)

main = interact (show . run . map (parse . words) . lines)
