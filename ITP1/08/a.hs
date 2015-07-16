import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BLC

swapCase :: Char -> Char
swapCase c
    | isUpper c = toLower c
    | isLower c = toUpper c
    | otherwise = c

main = BLC.getContents >>= BLC.putStr . BLC.map swapCase
