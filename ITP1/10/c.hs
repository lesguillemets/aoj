import Control.Applicative ((<$>))
import Control.Monad (unless)

main = do
    n <- readLn
    unless (n == 0) $ do
        ss <- map read . words <$> getLine
        print $ sd ss
        main

sd :: [Double] -> Double
sd ss = sqrt . (/n) . sum $ [ (si - m)^2 | si <- ss]
    where
        n = fromIntegral $ length ss
        m = (sum ss) / n
