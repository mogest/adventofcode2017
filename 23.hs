import qualified Data.Map as Map
import Data.Numbers.Primes
import Tablet (Instruction(MUL), State(..), execute, parse)

partA listing =
  length $
  filter runningMUL $ takeWhile (not . halt) $ execute program notimpl notimpl initialState
  where
    program = parse listing
    initialState = State Map.empty 0 False
    notimpl _ _ = error "not implemented"
    runningMUL state =
      (pc state >= 0) &&
      (pc state < length program) && isMUL (program !! pc state)
    isMUL MUL {} = True
    isMUL _ = False

partB = length $ filter (not . isPrime) $ take 1001 [107900,107917 ..]

run listing = (partA listing, partB)

main = interact $ show . run
