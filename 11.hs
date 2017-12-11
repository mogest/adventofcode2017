move :: (Int, Int) -> String -> (Int, Int)
move (j, k) "n"  = (j - 2, k)
move (j, k) "s"  = (j + 2, k)
move (j, k) "nw" = (j - 1, k - 1)
move (j, k) "sw" = (j + 1, k - 1)
move (j, k) "ne" = (j - 1, k + 1)
move (j, k) "se" = (j + 1, k + 1)

distance :: (Int, Int) -> Int
distance (j, k)
  | j' <= k'  = k'
  | otherwise = k' + (j' `div` 2)
  where
    j' = abs j
    k' = abs k

run :: [String] -> (Int, Int)
run directions = (last distances, maximum distances)
  where
    distances = map distance $ scanl move (0, 0) directions

decomma ',' = ' '
decomma c   = c

main = interact (show . run . words . map decomma)
