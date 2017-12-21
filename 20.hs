import Data.Char (isDigit)
import Data.Function (on)
import Data.List (groupBy, minimumBy, sortBy)

type Vector = (Int, Int, Int)

data Particle = Particle
  { p :: Vector
  , v :: Vector
  , a :: Vector
  } deriving (Show)

distance (x, y, z) = abs x + abs y + abs z

delta p1 p2 = abs $ distance (p p2) - distance (p p1)

removeCollisions particles =
  concat $
  filter single $ groupBy samePosition $ sortBy comparePosition particles
  where
    comparePosition a b = p a `compare` p b
    samePosition a b = p a == p b
    single a = length a == 1

tick particle =
  Particle (apply position particle) (apply velocity particle) (a particle)
  where
    position p v a = p + v + a
    velocity p v a = v + a
    apply f Particle {p = (px, py, pz), v = (vx, vy, vz), a = (ax, ay, az)} =
      (f px vx ax, f py vy ay, f pz vz az)

part2 particles =
  length $ iterate (removeCollisions . map tick) particles !! 500

part1 particles = fst $ minimumBy f $ zip [0 ..] particles
  where
    d point = compare `on` (distance . point)
    f (_, p1) (_, p2) = g (d a p1 p2) (d v p1 p2) (d p p1 p2)
    g EQ EQ c = c
    g EQ c _ = c
    g c _ _ = c

run particles = (part1 particles, part2 particles)

parse values = Particle (a, b, c) (d, e, f) (g, h, i)
  where
    [a, b, c, d, e, f, g, h, i] = map read values

ignoreUnnecessaryChar c
  | isDigit c || c == '-' = c
  | otherwise = ' '

main =
  interact
    (show . run . map (parse . words . map ignoreUnnecessaryChar) . lines)
