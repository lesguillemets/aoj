import Data.List (sortBy)
main = getContents >>= mapM_ print . take 3 . sortBy (flip compare) .
            map (read :: String -> Int) .  lines
