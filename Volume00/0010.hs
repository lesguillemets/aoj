import Text.Printf (printf)
import Data.Complex

format :: Double -> String
format = printf "%.3f"

type Triangle = (Complex Double, Complex Double)

solve :: [Double] -> [Double]
solve [x0,y0,x1,y1,x2,y2] = [px,py,r]
    where
        rv = csc ((x1-x0) :+ (y1-y0),(x2-x0) :+ (y2-y0))
        r = realPart $ abs rv
        (px :+ py) = (x0:+y0) + rv

csc :: Triangle -> Complex Double
csc (x0:+y0,b@(x1:+y1)) = b/2 + l * (y1 :+ (-x1)) where
    s0 = x0^2 + y0^2 - x0*x1 - y0*y1
    s1 = x0*y1 - x1*y0
    l = (s0/(2*s1)) :+ 0


main = getContents >>=
    mapM_ (putStrLn . unwords . map format . solve . map read . words)
        . tail . lines
