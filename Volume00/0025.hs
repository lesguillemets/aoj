hitBlow :: Eq a => [a] -> [a] -> (Int,Int)
hitBlow nums guess = (hits, blows) where
    hits = length . filter id $ zipWith (==) nums guess
    blows = subtract hits . length . filter (`elem` nums) $ guess

solve :: [[Int]] -> [(Int,Int)]
solve [] = []
solve [_] = []
solve (n:g:xs) = hitBlow n g : solve xs

main = getContents >>= mapM_ (\(h,b) -> putStrLn $ show h ++ " " ++ show b)
    . solve . map (map read . words) . lines
