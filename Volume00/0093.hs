import Control.Applicative
import Data.List (intercalate)

isLeap :: Int -> Bool
isLeap n
    | n `divisible` 400 = True
    | n `divisible` 100 = False
    | n `divisible` 4 = True
    | otherwise = False

divisible :: Int -> Int -> Bool
divisible n m = n `mod` m == 0

leaps :: Int -> Int -> [Int]
leaps a b = filter isLeap [a..b]

step :: IO [[Int]]
step = do
    [n,m] <- map read . words <$> getLine
    if n == 0 && m == 0
        then return []
        else (leaps n m :) <$> step

main = step >>= putStr . intercalate "\n" . map showYears

showYears :: [Int] -> String
showYears [] = "NA\n"
showYears ys = unlines . map show $ ys
