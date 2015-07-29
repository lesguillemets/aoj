pTa :: String -> String
pTa [] = []
-- somewhat weird
pTa v = case splitAt 5 v of
            ("apple",s) -> "peach" ++ pTa s
            ("peach", s) -> "apple" ++ pTa s
            _ -> let (pr, po) = break (\c -> (c == 'a') || (c == 'p')) (tail v)
                     in head v : pr ++ pTa po

main = getLine >>= putStrLn . pTa
