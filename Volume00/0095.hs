import Data.List

winner :: [(Int,Int)] -> (Int,Int)
winner = foldl1' (\w c -> case snd w `compare` snd c of
                             GT -> w
                             LT -> c
                             EQ -> min c w
                )

main = do
    _ <- getLine
    getContents >>= putStrLn . (\(n,f) -> show n ++ " " ++ show f)
        . winner . map ( (\(n:f:_) -> (n,f)) . map read . words) . lines
