import Control.Monad
thirdRoot :: Double -> Double
thirdRoot q = head . dropWhile ((> (q*threshold)) . abs . (q -) . (^3))
    $ iterate iter (q/2)
    where
        iter :: Double -> Double
        iter x = x - (x^3-q)/(3*x^2)

threshold = 0.00001

main = do
    n <- readLn
    unless (n == -1) $ do
        print . thirdRoot . fromIntegral $ n
        main
