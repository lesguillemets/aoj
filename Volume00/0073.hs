surfaceArea :: Double -> Double -> Double
surfaceArea x h = x^2 + 2*x* sqrt ((x/2)^2 + h^2 )

solve :: [Double] -> [Double]
solve [] = []
solve [_] = []
solve (0:0:_) = []
solve (x:y:r) = surfaceArea x y : solve r

main = getContents >>= mapM_ print . solve . map read . lines
