isPalindrome :: Eq a => [a] -> Bool
isPalindrome = (==) =<< reverse

main = getContents >>= print . length . filter isPalindrome . lines
