tsubo :: Double
tsubo = 3.305785
main = getLine >>= print . (/tsubo) . (\(x:y:_) -> x*y) . map read . words
