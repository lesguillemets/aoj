import Control.Applicative
-- using separate funtcions concering the accuracy of (**)
manhattanDist :: [Double] -> [Double] -> Double
manhattanDist xs ys = sum $ zipWith ((abs .) . (-)) xs ys

euclDist :: [Double] -> [Double] -> Double
euclDist xs ys = sqrt . sum $ zipWith (\x y -> (x-y)^2) xs ys

chebyshevDist :: [Double] -> [Double] -> Double
chebyshevDist xs ys = maximum $ zipWith ((abs . ) . (-)) xs ys

minkowskiDist :: Int -> [Double] -> [Double] -> Double
minkowskiDist n xs ys =
    (** (1/fromIntegral n)) . sum $ zipWith (\x y -> abs (x-y) ^n) xs ys

main = do
    _ <- getLine
    xs <- map read . words <$> getLine
    ys <- map read . words <$> getLine
    print $ manhattanDist xs ys
    print $ euclDist xs ys
    print $ minkowskiDist 3 xs ys
    print $ chebyshevDist xs ys
