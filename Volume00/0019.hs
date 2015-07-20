import Data.List (foldl1')
fact :: Int -> Int
fact n = foldl1' (*) [1..n]

main = readLn >>= print . fact

