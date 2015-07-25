import Control.Monad
import Data.List

toI :: String -> Int
toI = read . dropWhile (== '0')
d :: String -> Int
d s = (toI . sortBy (flip compare)) s - (toI . sort) s

main = do
    n <- readLn
    replicateM_ n $ getLine >>= print . d
