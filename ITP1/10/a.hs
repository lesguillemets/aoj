main = getLine >>= print . dist . map read . words

dist :: [Double] -> Double
dist [x0,y0,x1,y1] = sqrt $ (x1-x0)^2 + (y1-y0)^2
