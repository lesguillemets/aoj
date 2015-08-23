import Data.List
import Data.Maybe

gweights :: [Int]
gweights = reverse . take 10 . iterate (*2) $ 1

grandma :: Int -> [Int]
grandma = measure gweights

measure :: [Int] -> Int -> [Int]
measure weights x = catMaybes $ unfoldr f (weights,x) where
    f (_,0) = Nothing
    f ([],_) = Nothing
    f (w:ws, n) = if w <= n then Just (Just w, (ws,n-w))
                            else Just (Nothing, (ws,n))

main = getContents >>=
    mapM_ (putStrLn . unwords . map show . reverse . grandma . read) . lines
