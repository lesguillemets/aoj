import Control.Monad (unless)
main = getContents >>= f .  map (map read . words) . lines where
    f :: [[Int]] -> IO ()
    f [] = return ()
    f ([x,y]:re) = unless (x == 0 && y == 0) $ do
        putStrLn . unwords . map show $
            case x `compare` y of
                GT -> [y,x]
                _ -> [x,y]
        f re
