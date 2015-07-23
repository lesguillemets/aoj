decode :: String -> String
decode ('@':d:c:rest) = replicate (read . return $ d) c ++ decode rest
decode (c:rest) = c:decode rest
decode [] = []

main = getContents >>= mapM_ (putStrLn . decode) . lines
