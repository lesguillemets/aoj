import qualified Data.Vector.Unboxed as V
import Control.Monad

maxSumSeq :: V.Vector Int -> Int
maxSumSeq ns = f sums
    where
        sums = V.scanl' (+) 0 ns
        f ss
            | V.null ss = 0
            | otherwise  = let
                (pre, post) = V.splitAt (V.maxIndex ss) ss
                current = if V.null pre then 0 else V.head post - V.minimum pre
                in
                    max current (f (V.tail post))


main = do
    n <- readLn
    unless (n == 0) $ do
        replicateM n readLn >>= print . maxSumSeq . V.fromList
        main

