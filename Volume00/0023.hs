data Circle = Circle { _loc :: (Double, Double), _r :: Double }
distance :: Circle -> Circle -> Double
distance (Circle (x0,y0) _) (Circle (x1,y1) _) = sqrt $ (x0-x1)^2 + (y0-y1)^2

solve :: Circle -> Circle -> Int
solve c0@(Circle _ r0) c1@(Circle _ r1)
    | r0+r1 < d = 0
    | d+r1 < r0 = 2
    | d+r0 < r1 = -2
    | otherwise = 1
    where d = distance c0 c1

parseLn :: [Double] -> (Circle, Circle)
parseLn [xa,ya,ra,xb,yb,rb] = (Circle (xa,ya) ra , Circle (xb,yb) rb)

main = getContents >>=
    mapM_ (print . uncurry solve . parseLn . map read . words) . tail . lines


