import Text.Printf (printf)
-- don't use this signature at home
solve :: [Double] -> [Double]
solve [a,b,c,d,e,f] = let
    det = (a*e-b*d)
    inv = map (map (/det)) [[e,-b],[-d,a]]
    in
        map (sum . zipWith (*) [c,f]) inv

main = getContents >>=
    mapM_ (putStrLn . unwords . map (printf "%.3f") . solve . map read . words) . lines

