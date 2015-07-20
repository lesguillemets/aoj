import Control.Arrow
import Data.List

sclMult :: Num a => a -> (a,a) -> (a,a)
sclMult a = (*a) *** (*a)
addVect :: Num a => (a,a) -> (a,a) -> (a,a)
addVect (x,y) (z,w) = (x+z,y+w)

hunt :: [(Int,Int)] -> (Int,Int)
hunt = (truncate *** truncate) . fst . foldl' step ((0,0),90) where
    step :: ((Double,Double), Int) -> (Int,Int) -> ((Double,Double),Int)
    step (loc,dir) (d,theta) = (newloc,(dir-theta) `mod` 360) where
        t =  pi * fromIntegral dir / 180
        newloc = addVect loc . sclMult (fromIntegral d) $ (cos t, sin t)

main = getContents >>=
    (\(x,y) -> do print x; print y) . hunt
            . map ( (\[x,y] -> (x,y)) . map read . split) . lines


split :: String -> [String]
split [] = []
split xs = let (pre,post) = span (/= ',') xs
               in
                   if null post then [pre]
                                else pre: split (tail post)
