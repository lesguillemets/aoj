type Triangle = (Int,Int,Int)
isRightTriangle :: Triangle -> Bool
isRightTriangle (x,y,z) = let
    d = maximum [x,y,z]
    in
        x^2+y^2+z^2 == 2* d^2

readTriangle :: String -> Triangle
readTriangle s = let [p,q,r] = map read $ words s in (p,q,r)

main = do
    _ <- getLine
    getContents >>=
        mapM_ (putStrLn . (\b -> if b then "YES" else "NO") . isRightTriangle
                                    . readTriangle) . lines
