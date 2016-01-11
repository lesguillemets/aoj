{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
import Control.Applicative

selectMinimumBy :: (a -> a -> Ordering) -> [a] -> ([a],a,[a])
selectMinimumBy f (x:xs) = smb ([],x,xs)
    where
        smb cand [] = ([],cand)
        smb cand (y:ys) = case f cand y of
                               LT -> y <:> smb cand ys
                               _ -> cand <:> smb y ys
selectMinimum :: Ord a => [a] -> ([a],a)
selectMinimum = selectMinimumBy compare

selectionSort :: Ord a => [a] -> ([a],Int)
selectionSort = selectionSort' . (,0)

-- |
-- >>> selectionSort ([5,6,4,2,1,3] :: [Int])
-- ([1,2,3,4,5,6],4)
-- >>> selectionSort ([5,2,4,6,1,3] :: [Int])
-- ([1,2,3,4,5,6],3)
 
-- TODO : find a good higer order function>
selectionSort' :: Ord a => ([a],Int) -> ([a],Int)
selectionSort' ([],n) = ([],n)
selectionSort' (xs,n) = let (rs,h) = selectMinimum xs
                            in
                            h <:> if h /= head xs
                                     then selectionSort' (rs, n+1)
                                     else selectionSort' (rs,n)
-- |

(<:>) x (xs,b) = (x:xs,b)
main = do
    _ <- getLine
    ns <- map (read :: String -> Int) . words <$> getLine
    let (sorted,n) = selectionSort ns
    putStrLn . unwords . map show $ sorted
    print n

