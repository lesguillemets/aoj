{-# LANGUAGE ScopedTypeVariables #-}
import Control.Applicative

main = do
    [w,h,x,y,r] :: [Int] <- map read . words <$> getLine
    putStrLn $ if 0 <= x-r && x+r <= w && 0 <= y-r && y+r <= h
                   then "Yes"
                   else "No"
