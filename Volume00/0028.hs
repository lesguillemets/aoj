import Control.Applicative
import qualified Data.IntMap.Strict as I
import Data.List
import Data.Function (on)

main = do
    nums <- foldl' (flip (flip (I.insertWith (+)) 1)) I.empty
        . map read . lines <$> getContents
    let ns = sortBy (flip compare `on` snd) . I.toAscList $ nums
        maxFreq = snd . head $ ns
    mapM_ (print . fst) . takeWhile ((maxFreq ==) . snd) $ ns

