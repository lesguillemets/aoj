{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
import Data.Function
import Data.List
main = do
    _ <- getLine
    cards <- map fromString . words <$> getLine
    let sSorted = fst $ selectionSort cards
        bSorted = snd $ bubbleSort cards
    putStrLn . unwords . map show $ bSorted
    putStrLn $ if stableSorted cards bSorted then "Stable" else "Not stable"
    putStrLn . unwords . map show $ sSorted
    putStrLn $ if stableSorted cards sSorted then "Stable" else "Not stable"

data Suite = Heart | Club | Spade | Diamond deriving (Show, Eq)
fromChar :: Char -> Suite
fromChar 'H' = Heart
fromChar 'C' = Club
fromChar 'S' = Spade
fromChar 'D' = Diamond

data Card = Card { _suite :: Suite, _num :: Int} deriving (Eq)
fromString :: String -> Card
fromString (s:num) = Card (fromChar s) (read num)

instance Ord Card where
    compare = compare `on` _num

instance Show Card where
    show (Card s n) = (head $ show s) : show n

stableSorted :: [Card] -> [Card] -> Bool
stableSorted orig = all sameOrder . groupBy ((==) `on` _num)
    where
        sameOrder = ascending . map (`elemIndex` orig)
        ascending xs = all (==LT) $ zipWith compare xs (tail xs)

-- |
-- >>> stableSorted 

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
                              rs = take (i-1) xs ++ x:drop i xs
                            in
                            h <:> if i /= 0
                                     then selectionSort' (rs, n+1)
                                     else selectionSort' (xs,n)

bubbleSort :: Ord a => [a] -> (Int,[a])
bubbleSort = bubbleSort' . (0,)

bubbleSort' :: Ord a => (Int, [a]) -> (Int,[a])
bubbleSort' (n,xs) =
    let (fl,n',xs') = f False n xs
        f :: Ord a => Bool -> Int -> [a] -> (Bool,Int,[a])
        f b m [] = (b,m,[])
        f b m [x] = (b,m,[x])
        f b m (x:y:rest) = if x > y
                              then y <::> f True (m+1) (x:rest)
                              else x <::> f b m (y:rest)
        in
        if fl then bubbleSort' (n',xs')
              else (n',xs')


(<:>) x (xs,b) = (x:xs,b)
(<::>) :: forall t t1 a. a -> (t, t1, [a]) -> (t, t1, [a])
(<::>) x (a,b,xs) = (a,b,x:xs)
-- }}}
-- vim:fdm=marker
