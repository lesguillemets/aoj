import Data.List
import Data.Maybe

data Result = O | X | Draw
instance Show Result where
    show O = "o"
    show X = "x"
    show Draw = "d"


ttt :: [Char] -> Result
ttt xs = let
    linesToCheck =
        cutEvery 3 xs ++ (transpose . cutEvery 3) xs ++
        map (map (xs !!)) [[0,4,8], [2,4,6]]
    lns = map checkLine linesToCheck
    in
        case (listToMaybe . catMaybes) lns of
            Nothing -> Draw
            (Just r) -> r

checkLine :: [Char] -> Maybe Result
checkLine xs
    | all (== 'o') xs = Just O
    | all (== 'x') xs = Just X
    | otherwise       = Nothing

-- try weird things
cutEvery :: Int -> [a] -> [[a]]
cutEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)

main = getContents >>= mapM_ (print . ttt) . lines
