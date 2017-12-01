import Data.Char

rotate 0     list = list
rotate count list = rotate (count - 1) (tail list ++ [head list])

pairedDigits digits count = zip digits $ rotate count digits

calculateCaptcha captcha count =
    sum [digitToInt first | (first, second) <- pairedDigits captcha count, first == second]

main = do
    captcha <- getLine

    putStrLn $ show $ calculateCaptcha captcha 1
    putStrLn $ show $ calculateCaptcha captcha (length captcha `div` 2)
