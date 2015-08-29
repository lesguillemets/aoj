
-- for safety(?)
floorThird :: Integral a => a -> a
floorThird n
        | n' == n = third
        | n' < n = if (third+1)^3 <= n
                            then third+1
                            else third
        | n' > n = third-1
        where third = floor $ (fromIntegral n)**(1/3)
              n' = third^3

solve :: Int -> Int
solve z = z^3 - maximum (cands z)

cands :: Int -> [Int]
cands z = [x3+y^3 | x3 <- map (^3) [1..z-1], let y = floorThird (z^3-x3)]

main = getContents >>=
        mapM_ (print . solve) . takeWhile (/= 0) . map read . lines
