import KnotHash

main = interact (toHex . knotHash . head . lines)
