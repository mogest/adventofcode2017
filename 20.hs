import Data.Function (on)
import Data.List (minimumBy)

type Vector = (Int, Int, Int)

data Particle = Particle { p :: Vector
                         , v :: Vector
                         , a :: Vector
                         } deriving Show

part2 particles = 0

part1 particles = fst $ minimumBy f $ zip [0..] particles
  where
    distance (x, y, z) = (abs x) + (abs y) + (abs z)
    d point = compare `on` (distance . point)
    f (_, p1) (_, p2) = g (d a p1 p2) (d v p1 p2) (d p p1 p2)
    g EQ EQ c = c
    g EQ c  _ = c
    g c  _  _ = c

run particles = (part1 particles, part2 particles)

parse values = Particle (a,b,c) (d,e,f) (g,h,i)
  where
    (a:b:c:d:e:f:g:h:i:[]) = map read values

ignoreUnnecessaryChar c
  | c >= '0' && c <= '9' || c == '-'  = c
  | otherwise                         = ' '

main = interact (show . run . map (parse . words . map ignoreUnnecessaryChar) . lines)
