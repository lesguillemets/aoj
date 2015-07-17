import Control.Applicative
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromJust)

isPrime :: Integral a => a -> Bool
isPrime n = let m = floor . sqrt $ fromIntegral n
              in
                  all ((/= 0) . mod n) [2..m]

readInts :: IO [Int]
readInts = map (fst . fromJust . BC.readInt) . BC.lines <$> BC.getContents

main = do
    _ <- getLine
    readInts >>= print . length . filter isPrime
