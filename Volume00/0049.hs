{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
import Data.List
data Class = Class {_a :: Int, _b :: Int, _ab :: Int, _o :: Int}
data BT = A | B | AB | O deriving (Read,Show)
emptyClass :: Class
emptyClass = Class 0 0 0 0

toBT :: String -> BT
toBT = read . tail . dropWhile (/= ',')

addPerson :: Class -> String -> Class
addPerson (c@Class{..}) dat =
        case toBT dat of
            A -> c {_a = succ _a}
            B -> c {_b = succ _b}
            AB -> c {_ab = succ _ab}
            O -> c {_o = succ _o}

say :: Class -> IO()
say (Class a b o ab) = mapM_ print [a,b,o,ab]

main = getContents >>= say . foldl' addPerson emptyClass . lines
