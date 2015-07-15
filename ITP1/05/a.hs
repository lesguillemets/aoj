import Control.Monad

drawRect :: Int -> Int -> IO ()
drawRect h w = replicateM_ h (putStrLn $ replicate w '#')

main = getContents >>= mapM_ (\[h,w] -> do drawRect h w; putStrLn "") .
    takeWhile (/= [0,0]) . map (map read . words) . lines

