import qualified Data.Map as Map
import qualified Data.Sequence as Seq

type Banks = Seq.Seq Int

recompute :: Banks -> Banks
recompute banks =
  go (nextIndex index) blocks (Seq.update index 0 banks)
  where
    blocks                = maximum banks
    Just index            = Seq.elemIndexL blocks banks
    go index 0     banks  = banks
    go index count banks  = go (nextIndex index) (count - 1) (increment index banks)
    increment index banks = Seq.update index (banks `Seq.index` index + 1) banks
    nextIndex index       = if Seq.length banks == index + 1 then 0 else index + 1

compute :: Banks -> (Int, Int)
compute banks =
  go 0 banks Map.empty
  where
    go count banks seen =
      case Map.lookup banks seen of
        Just loopCount -> (count, count - loopCount)
        Nothing        -> go (count + 1) (recompute banks) (Map.insert banks count seen)

run :: [Int] -> (Int, Int)
run banks = compute $ Seq.fromList banks

main = interact (show . run . map read . words)
