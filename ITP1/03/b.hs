main = getContents >>= f . zip [1..] . lines where
    f [] = return ()
    f ((i,x):re) =
        case x of
            "0" -> return ()
            _ -> do
                putStrLn $ "Case " ++ show i ++ ": " ++ x
                f re
