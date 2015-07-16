import Control.Applicative
import Data.List (isPrefixOf)

main = containsLoop <$> getLine <*> getLine >>=
        \i -> putStrLn $ if i then "Yes" else "No"

containsLoop :: Eq a => [a] -> [a] -> Bool
containsLoop s = contains (s++s)

contains :: Eq a => [a] -> [a] -> Bool
contains [] _ = False
contains src sub = (sub `isPrefixOf` src) || contains (tail src) sub
