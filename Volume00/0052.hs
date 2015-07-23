import Control.Monad (unless)

factTrailZeros :: Int -> Int
factTrailZeros n = sum . takeWhile (/= 0) . map (n `div`) $ iterate (*p) p
    where p = 5

main = do
    n <- readLn
    unless (n == 0) $ do
        print $ factTrailZeros n
        main
