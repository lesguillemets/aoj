-- well, gcd is defined in GHC.Real and is imported in Prelude.
gcd' :: Integral a => a -> a -> a
gcd' x y = if x >= y then gcd'' x y
                     else gcd'' y x where
                         gcd'' a 0 = a
                         gcd'' a b = gcd' b (a`mod`b)

main = getLine >>= print . (\[x,y] -> gcd' x y) . map read . words
