import qualified Data.ByteString.Char8 as BC
import Data.Maybe

readInts :: BC.ByteString -> [Int]
readInts = map (fst . fromJust . BC.readInt) . BC.words

solve :: [Int] -> IO ()
solve ns = do
    let mn = minimum ns
        mx = maximum ns
        s = sum ns
    putStrLn . unwords . map show $ [mn,mx,s]

main = do
    _ <- getLine
    BC.getLine >>= solve . readInts
