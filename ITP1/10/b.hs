import Control.Applicative
main = do
    [a,b,c] <- map read . words <$> getLine
    let
        c' = c*pi/180
    print $ a*b*sin c' / 2
    print $ a + b + sqrt (a^2 + b^2 - 2*a*b*cos c')
    print $ b*sin c'
