{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
import Control.Applicative

findMinimum :: Ord a => [a] -> (a,Int)
findMinimum = minimum . flip zip [0..]


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
selectionSort' (x:xs,n) = let (h,i) = findMinimum (x:xs)
                              rs = take (i-1) xs ++ x:drop(i) xs
                            in
                            h <:> if i /= 0
                                     then selectionSort' (rs, n+1)
                                     else selectionSort' (xs,n)
-- |

(<:>) x (xs,b) = (x:xs,b)
main = do
    _ <- getLine
    ns <- map (read :: String -> Int) . words <$> getLine
    let (sorted,n) = selectionSort ns
    putStrLn . unwords . map show $ sorted
    print n

