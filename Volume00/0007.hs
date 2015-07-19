import Data.List

week :: Int -> Int
week = ceiling . (*1.05) . fromIntegral

main = do
    n <- readLn
    -- I believe there's some nice function for this..
    print . (*1000) $ foldl' (flip (const week)) 100 (replicate n 0)
