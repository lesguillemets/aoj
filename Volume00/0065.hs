import Control.Applicative
import qualified Data.IntMap.Strict as I
import Data.IntMap.Strict (IntMap)
import System.IO (isEOF)

readTrades :: IntMap Int -> IO (IntMap Int)
readTrades m = do
    c <- isEOF
    if c then return m
         else do
            l <- getLine
            case l of
                "" -> return m
                ss -> readTrades (I.insertWith (+) custNum 1 m)
                    where
                        custNum = read . takeWhile (/= ',') $ ss

main = do
    lastMonth <- readTrades I.empty
    thisMonth <- readTrades I.empty
    mapM_ (\(i,t) -> putStrLn . unwords . map show $ [i,t]) .  I.toAscList $
        I.intersectionWith (+) thisMonth lastMonth
