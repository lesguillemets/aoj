import Control.Arrow
import qualified Data.ByteString.Char8 as BC
import Data.List
import Data.Function (on)
import Data.Char (toLower)

-- not efficient at all, but the input is not very long.
stat :: BC.ByteString -> (BC.ByteString,BC.ByteString)
stat = ((head . maximumBy (compare `on` length))
        &&&
        (head . maximumBy (compare `on` (BC.length . head)))
       ) .  group . sort . BC.words . BC.map toLower

main = BC.getLine >>= (\(x,y) -> BC.putStrLn . BC.unwords $ [x,y]) . stat
