import Data.List (sort)
main = getLine >>=
    putStrLn . unwords . map show . (sort::[Int] -> [Int]) . map read . words
