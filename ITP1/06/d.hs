import Control.Applicative
import Control.Monad
mul :: Num a => [[a]] -> [a] -> [a]
mul xs as = map (sum . zipWith (*) as) xs

main = do
   [n,m] <- map read . words <$> getLine
   matr <- replicateM n (map read . words <$>  getLine)
   v <- replicateM m readLn
   mapM_ print $ mul matr v
