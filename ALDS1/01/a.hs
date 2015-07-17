insertionSortVerbose :: Ord a => [a] -> [[a]]
insertionSortVerbose xs = scanl step xs [0..length xs-1]
    where
        step es n = let
            (pre,x:post) = splitAt n es
            in
                insertOrderd x pre ++ post

insertOrderd :: Ord a => a -> [a] -> [a]
insertOrderd x ts = let (pre,post) = span (< x) ts
                        in
                            pre ++ (x:post)


main = do
    _ <- getLine
    getLine >>= mapM_ (putStrLn . unwords . map show) . tail .
        insertionSortVerbose . map (read :: String -> Int) . words
