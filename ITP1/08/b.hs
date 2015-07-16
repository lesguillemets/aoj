main = do
    n <- getLine
    case n of
        "0" -> return ()
        _ -> do
            print . sum . map (read . return) $ n
            main
