{-# LANGUAGE ScopedTypeVariables #-}
import Control.Applicative
main = do
    [a,b]::[Int] <- map read . words <$> getLine
    putStrLn . unwords . map show $ [a*b,2*(a+b)]

