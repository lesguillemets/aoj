-- using the gcd and lcm in prelude

main = getContents >>= mapM_ (\[x,y] -> putStrLn . unwords . map show
                                $ [gcd x y , lcm x y]
                                ) . map (map read . words) . lines

