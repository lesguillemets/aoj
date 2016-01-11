import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))
import Data.Maybe

findBinary :: V.Vector Int -> Int -> Bool
findBinary v n = fB (0, V.length v)
    where
        fB (lower,upper)
            | upper - lower <= 1 = lower == n || upper == n
            | otherwise = let mid = (upper + lower) `div` 2
                              x = v ! mid
                              in
                               case n `compare` x of
                                    LT -> fB (lower,mid)
                                    GT -> fB (mid,upper)
                                    EQ -> True
-- |
-- >>> let v = V.fromList [1,3,4,5,8,12,13]
-- >>> findBinary v 1
-- True
-- >>> findBinary v 11
-- False
-- >>> findBinary v 13
-- True

readInts :: IO [Int]
readInts = map (fst . fromJust . BC.readInt) . BC.words <$> BC.getLine

main = do
    _ <- getLine
    s <- V.fromList <$> readInts
    _ <- getLine
    t <- V.fromList <$> readInts
    print . V.length . V.filter (findBinary s) $ t
