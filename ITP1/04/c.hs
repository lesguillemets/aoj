import Control.Applicative
import Data.Maybe

calc :: String -> Maybe Int
calc s = let
    [a',op,b'] = words s
    a = read a'
    b = read b'
    in
        case op of
            "+" -> Just $ a+b
            "-" -> Just $ a-b
            "*" -> Just $ a*b
            "/" -> Just $ a `div` b
            _ -> Nothing


main = getContents >>=
    mapM_ (print . fromJust) . takeWhile isJust . map calc . lines
