numDigits :: Int -> Int
numDigits = length . show

main = getContents >>=
    mapM_ (print . numDigits . (\[x,y] -> x+y) . map read . words) . lines
