import Control.Applicative
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromJust)

readInts :: IO (S.Set Int)
readInts =
    S.fromList . map (fst . fromJust . BC.readInt) . BC.words <$> BC.getLine

main = do
    _ <- getLine
    ss <- readInts
    _ <- getLine
    ts <- readInts
    print . S.size $ ss `S.intersection` ts
