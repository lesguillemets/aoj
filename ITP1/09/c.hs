import Data.List (foldl')
type Score = (Int,Int)
turn :: Score -> [String] -> Score
turn (x,y) [s0,s1] = case s0 `compare` s1 of
                         GT -> (x+3,y)
                         LT -> (x,y+3)
                         EQ -> (x+1,y+1)

main = do
    _ <- getLine
    getContents >>=
        (\(x,y) -> putStrLn . unwords . map show $ [x,y]) .
            foldl' turn (0,0) . map words . lines
