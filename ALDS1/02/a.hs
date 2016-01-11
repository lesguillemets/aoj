{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
import Control.Applicative

bubbleSort :: Ord a => [a] -> (Int,[a])
bubbleSort = bubbleSort' . (0,)

bubbleSort' :: Ord a => (Int, [a]) -> (Int,[a])
bubbleSort' (n,xs) =
    let (fl,n',xs') = f False n xs
        f :: Ord a => Bool -> Int -> [a] -> (Bool,Int,[a])
        f b m [] = (b,m,[])
        f b m [x] = (b,m,[x])
        f b m (x:y:rest) = if x > y
                              then y <:> f True (m+1) (x:rest)
                              else x <:> f b m (y:rest)
        in
        if fl then bubbleSort' (n',xs')
              else (n',xs')

(<:>) :: forall t t1 a. a -> (t, t1, [a]) -> (t, t1, [a])
(<:>) x (a,b,xs) = (a,b,x:xs)

main = do
    _ <- getLine
    ns <- map (read :: String -> Int) . words <$> getLine
    let (n,sorted) = bubbleSort ns
    putStrLn . unwords . map show $ sorted
    print n

