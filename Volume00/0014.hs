integral :: Int -> Int
integral d = (*d) . sum . map f $ [d,d+d..600-d] where f = (^2)

main = getContents >>= mapM_ (print . integral . read) . lines
