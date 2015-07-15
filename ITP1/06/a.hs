main = do
    _ <- getLine
    getLine >>= putStrLn . unwords . reverse . words

