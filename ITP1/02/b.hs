import Control.Applicative
main = do
    [a,b,c] <- map read . words <$> getLine
    putStrLn $ if (a::Int) < b && b < c
                   then "Yes"
                   else "No"

