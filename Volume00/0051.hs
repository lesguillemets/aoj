import Control.Monad
import Data.List

toI :: String -> Int
toI s = let nums = dropWhile (== '0') s in
    if null nums then 0
                 else read s
d :: String -> Int
d s = (toI . sortBy (flip compare)) s - (toI . sort) s

main = do
    n <- readLn
    replicateM_ n $ getLine >>= print . d
