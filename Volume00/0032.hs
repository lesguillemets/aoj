import Data.List
import Control.Applicative

type Shape =(Int,Int,Int)

isRombus :: Shape -> Bool
isRombus (a,b,_) = a == b

isRect :: Shape -> Bool
isRect (a,b,d) = a^2 + b^2 == d^2

splitWith :: Eq a => a -> [a] -> [[a]]
splitWith c = unfoldr f where
    f [] = Nothing
    f xs = let (pre,post) = break (== c) xs in
        Just (pre, drop 1 post)


parseLn :: String -> Shape
parseLn = (\(a:b:d:_) -> (a,b,d)) . map read . splitWith ','

main = do
    shapes <- map parseLn . lines <$> getContents
    let rects = length . filter isRect $ shapes
        rombs = length . filter isRombus $ shapes
    print rects
    print rombs


