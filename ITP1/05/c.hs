drawBoard :: Int -> Int -> IO ()
drawBoard h w = sequence_ $
    take h . cycle . map (putStrLn . take w) $ [cycle "#.", cycle ".#"]

main = getContents >>= mapM_ (\[h,w] -> do drawBoard h w; putStrLn "") .
        takeWhile (/= [0,0]) . map (map read . words) . lines

