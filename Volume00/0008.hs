main = getContents >>= mapM_ (print . length . solve . read) . lines

-- pretty brainless
solve :: Int -> [(Int,Int,Int)]
solve n = [(i,j,k) | i <- [0..9], j <- [0..9], k <- [0..9],
                        let l = n - i - j - k, 0 <= l && l <= 9]
