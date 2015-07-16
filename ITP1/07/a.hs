import Data.Maybe

type Grade = String

grade :: Int -> Int -> Int -> Maybe Grade
grade (-1) (-1) (-1) = Nothing
grade m f r
    | m == -1 || f == -1 = Just "F"
    | s >= 80 = Just "A"
    | s >= 65 = Just "B"
    | s >= 50 = Just "C"
    | s >= 30 = Just (if r >= 50 then "C" else "D")
    | otherwise = Just "F"
    where s = m+f

main = getContents >>=
        mapM_ (putStrLn . fromJust) . takeWhile isJust .
            map ((\[m,f,r] -> grade m f r) . map read . words) . lines
