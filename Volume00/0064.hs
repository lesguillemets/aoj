import Data.Char
nums :: String -> [Int]
nums [] = []
nums str = let (n,rest) = span isDigit . dropWhile (not . isDigit) $ str
               in case n of [] -> []
                            _ -> read n : nums rest

main = getContents >>= print . sum . nums
