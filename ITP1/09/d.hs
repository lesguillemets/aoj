import Control.Applicative
import Control.Monad
import Data.List (foldl')

doOrder :: [String] -> String -> IO String
doOrder (order:args) s =
    case order of
        "print" -> do
            let [a,b] = map read args
            putStrLn . take (b-a+1) . drop a $ s
            return s
        "reverse" -> do
            let
                [a,b] = map read args
                (pre,m) = splitAt a s
                (mid,rest) = splitAt (b-a+1) m
            return $ pre ++ reverse mid ++ rest
        "replace" -> do
            let
                [a,b] = map read . take 2 $ args
                (pre,m) = splitAt a s
                rest = drop (b-a+1) m
            return $ pre ++ last args ++ rest

main = do
    source <- getLine
    n <- readLn
    replicateM n getLine >>=
        flip (foldl' (>=>) return) source . map (doOrder . words)
    return ()



