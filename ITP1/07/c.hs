addSumInfo :: [[Int]] -> [[Int]]
addSumInfo = addColumnSums . map (\xs -> xs ++ [sum xs]) where
    addColumnSums :: [[Int]] -> [[Int]]
    addColumnSums t = t ++ [[sum [c!!i | c <- t] | i <- [0..length (head t) -1]]]
printTable :: [[Int]] -> IO ()
printTable = mapM_ (putStrLn . unwords . map show)

main = do
    _ <- getLine
    getContents >>= printTable . addSumInfo . map (map read . words) . lines
