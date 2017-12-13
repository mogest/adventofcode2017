import Control.Arrow

type Firewall = [(Int, Int)]

violations :: Firewall -> Int -> Firewall
violations firewall offset = filter move firewall
  where
    move (layer, range) = scanner == 0
      where
        position        = layer + offset
        fullPeriod      = position `mod` (range * 2 - 2)
        (half, period)  = fullPeriod `divMod` (range - 1)
        scanner         = if half == 0 then period else range - period - 1

successfulOffset :: Firewall -> Int
successfulOffset firewall =
  fst $
  head $
  dropWhile (not . null . snd) $
  map (id &&& violations firewall) [0 ..]

severity :: Firewall -> Int
severity firewall = sum $ map (\(a, b) -> a * b) $ violations firewall 0

run :: Firewall -> (Int, Int)
run firewall = (severity firewall, successfulOffset firewall)

parse :: [String] -> (Int, Int)
parse (depth:range:[]) = (read depth, read range)

main = interact (show . run . map (parse . words) . lines . filter (/= ':'))
