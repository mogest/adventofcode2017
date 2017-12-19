import Data.List.Split

startingPrograms = ['a' .. 'p']

data Move = Spin     { count :: Int }
          | Exchange { ai :: Int, bi :: Int }
          | Partner  { an :: Char, bn :: Char }
          deriving Show

dance programs Spin {count=count} = go programs count
  where
    go programs 0 = programs
    go programs count = go ((last programs):(init programs)) (count - 1)

dance programs Exchange {ai=ai, bi=bi} = map swap $ zip [0 ..] programs
  where
    swap (i, program)
      | i == ai   = b
      | i == bi   = a
      | otherwise = program
    a = programs !! ai
    b = programs !! bi

dance programs Partner {an=an, bn=bn} = map swap programs
  where
    swap program
      | program == an  = bn
      | program == bn  = an
      | otherwise      = program

fullDance = foldl dance

partA moves = fullDance startingPrograms moves

cycleLength moves = (+) 1
  $ length
  $ takeWhile (/= startingPrograms)
  $ drop 1
  $ scanl fullDance startingPrograms
  $ repeat moves

partB moves = foldl fullDance startingPrograms $ replicate count moves
  where
    count = 1000000000 `mod` cycleLength moves

run moves = (partA moves, partB moves)

parse ('s':number) = Spin (read number)
parse ('x':rest)   = Exchange (read a) (read b) where [a, b] = splitOn "/" rest
parse ('p':rest)   = Partner (head a) (head b) where [a, b] = splitOn "/" rest

main = interact (show . run . map parse . splitOn ",")
