import Control.Applicative
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromJust)
import Data.List

maxDiff :: (Ord a , Num a)=> [a] -> a
maxDiff xs =  fst $ foldl'
    (\(ma',mi') n -> (max ma' (n-mi'), min mi' n)) (- (head xs),head xs) (tail xs)

readInts :: IO [Int]
readInts = map (fst . fromJust . BC.readInt) . BC.lines <$> BC.getContents

main = do
    _ <- getLine
    readInts >>= print . maxDiff
