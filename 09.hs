garbage :: Int -> String -> (Int, String)
garbage count ('!':_:t) = garbage count t
garbage count ('>':t)   = (count, t)
garbage count (_:t)     = garbage (count + 1) t

parse :: Int -> Int -> Int -> String -> (Int, Int)
parse score nest chars ('{':t) = parse score (nest + 1) chars t
parse score nest chars (',':t) = parse score nest chars t
parse score nest chars ('<':t) = parse score nest (chars + chars') t'
  where (chars', t') = garbage 0 t
parse score nest chars ('}':t) = parse (score + nest) (nest - 1) chars t
parse score nest chars ""      = (score, chars)

run :: String -> (Int, Int)
run = parse 0 0 0

main = interact (show . run . head . lines)
