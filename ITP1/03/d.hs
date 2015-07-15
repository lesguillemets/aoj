main = getLine >>= print . (\[x,y,z] -> solve x y z) . map read . words

solve :: Int -> Int -> Int -> Int
solve a b c = length . filter ((== 0) . mod c) $ [a..b]
