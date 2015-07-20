runRail :: [Int] -> [Int]
runRail = runWithStack [] where
    runWithStack _ [] = []
    runWithStack s (x:xs) = case x of
                                0 -> head s : runWithStack (tail s) xs
                                _ -> runWithStack (x:s) xs

main = getContents >>= mapM_ print . runRail . map read . lines
