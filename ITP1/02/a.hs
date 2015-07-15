{-# LANGUAGE ScopedTypeVariables #-}
import Control.Applicative
main = do
    [a,b]::[Int] <- map read . words <$> getLine
    putStrLn $ case a `compare` b of
                   GT -> "a > b"
                   EQ -> "a == b"
                   LT -> "a < b"
