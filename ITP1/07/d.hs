import Data.List (transpose)
import Control.Applicative
import Control.Monad
mult :: Num a => [[a]] -> [[a]] -> [[a]]
mult a b' = let b = transpose b' in
    map (\l ->  map (sum . zipWith (*) l) b ) a

readMatrix :: Int -> IO [[Int]]
readMatrix n = map (map read .words) <$> replicateM n getLine

main = do
    [n,m,l] <- map read . words <$> getLine
    a <- readMatrix n
    b <- readMatrix m
    mapM_ (putStrLn . unwords . map show) $ mult a b

