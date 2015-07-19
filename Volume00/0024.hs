g :: Double
g = 9.8

requiredHeight :: Double -> Double
requiredHeight v = (v^2)/(2*g)

toFloor :: Double -> Int
toFloor h = 1 + ceiling (h/5)

main = getContents >>= mapM_ (print . toFloor . requiredHeight . read) . lines
