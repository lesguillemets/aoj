maxDiff :: (Num a, Ord a) => [a] -> a
maxDiff (x:xs) = iter (x,x) xs where
    iter (h,l) [] = h - l
    iter (h,l) (m:ms)
        | m > h = iter (m,l) ms
        | l > m = iter (h,m) ms
        | otherwise = iter (h,l) ms

main = getContents >>= print . maxDiff . map (read :: String -> Double) . lines
