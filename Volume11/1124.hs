import Control.Monad
import qualified Data.IntMap.Strict as I
import Data.List
import Data.Function (on)

convenients :: [[Int]] -> I.IntMap Int
convenients = foldl' (foldl' (\m d -> I.insertWith (+) d 1 m)) I.empty

fixDate :: Int -> I.IntMap Int -> Int
fixDate quorum m = case sortBy (flip compare `on` snd)
                        . filter ((>= quorum) . snd) . I.toAscList $ m of
                       [] -> 0
                       (n:_) -> fst n

main :: IO ()
main = do
    (n:q:_) <- map read . words <$> getLine
    unless (n == 0 && q == 0) $ do
        replicateM n getLine >>=
            print . fixDate q . convenients . map (map read . tail . words)
        main
