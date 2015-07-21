import Data.List (sortBy)
main = getLine >>= putStrLn . unwords . map show . sortBy (flip compare)
        . map (read :: String -> Int) . words
