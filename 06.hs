import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as V

type Banks = V.Vector Int

balance :: Banks -> Banks
balance banks =
  V.accum (+) banks updates
    where
      blocks     = V.maximum banks
      Just index = V.elemIndex blocks banks
      indexes    = take blocks $ drop (index + 1) $ cycle [0 .. V.length banks - 1]
      updates    = (index, -blocks) : zip indexes (repeat 1)

reallocate :: Banks -> (Int, Int)
reallocate banks =
  go 0 banks Map.empty
    where
      go count banks seen =
        case Map.lookup banks seen of
          Just loopCount -> (count, count - loopCount)
          Nothing        -> go (count + 1) (balance banks) (Map.insert banks count seen)

run :: [Int] -> (Int, Int)
run banks = reallocate $ V.fromList banks

main = interact (show . run . map read . words)
