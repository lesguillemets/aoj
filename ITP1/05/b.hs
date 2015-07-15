import Control.Monad

drawFrame :: Int -> Int -> IO ()
drawFrame h w = do
    putStrLn $ replicate w '#'
    replicateM_ (h-2) $ do
        putChar '#'
        putStr $ replicate (w-2) '.'
        putStrLn "#"
    putStrLn $ replicate w '#'

main = getContents >>= mapM_ (\[h,w] -> do drawFrame h w; putStrLn "") .
    takeWhile (/= [0,0]) . map (map read . words) . lines

