type Vect = (Double, Double)
line :: Vect -> Vect -> Vect
line (x,y) (a,b) = (x-a, y-b)

threshold :: Double
threshold = 0.0000001

isParallel :: Vect -> Vect -> Bool
isParallel (0,_) (u1,_) = u1 == 0
isParallel (u0,_) (0,_) = u0 == 0
isParallel (u0,v0) (u1,v1) = let
    d0 = v0/u0
    d1 = v1/u1
    in
        abs (d0 - d1) < threshold

main = getContents >>=
        mapM_ (putStrLn . (\b -> if b then "YES" else "NO")
                    . uncurry isParallel
                    . (\[ax,ay,bx,by,cx,cy,dx,dy] ->
                            (line (ax,ay) (bx,by), line (cx,cy) (dx,dy)))
                    . map read . words) . tail . lines
