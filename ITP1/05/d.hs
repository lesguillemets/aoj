call :: Int -> [Int]
call n = f 1
    where
        f x
            | x > n = []
            | x `mod` 3 == 0 = x: f (x+1)
            | any ((== 3) . (`mod` 10)) . takeWhile (/= 0 ) . iterate (`div` 10) $ x
                = x: f(x+1)
            | otherwise = f (x+1)

main = readLn >>= putStrLn . concatMap ((' ':) . show) . call
