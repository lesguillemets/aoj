import Control.Monad (replicateM)
import Data.List (foldl')

shuffle :: [a] -> Int -> [a]
shuffle li a = let (pre,post) = splitAt a li in post ++ pre

main = do
    source <- getLine
    case source of
        "-" -> return ()
        s -> do
            n <- readLn
            replicateM n readLn >>= putStrLn . foldl' shuffle s
            main
