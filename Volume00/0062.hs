bottomMost :: [Int] -> Int
bottomMost [n] = n
bottomMost ns = bottomMost $ zipWith (\n m -> (n+m) `mod` 10) ns (tail ns)

main = getContents >>= mapM_ (print . bottomMost . map (read . return)) . lines
