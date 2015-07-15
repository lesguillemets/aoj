import Control.Applicative
import Text.Printf (printf)
main = do
    [a,b] <- map read . words <$> getLine
    -- without using brain
    let (d,r) = a `divMod` b
    putStr $ show d
    putChar ' '
    putStr $ show r
    putChar ' '
    putStrLn . printf "%.6f" $ (fromIntegral a ::Double) / fromIntegral b
