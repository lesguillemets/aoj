import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

lot :: Int -> [(Int,Int)] -> V.Vector Int
lot n ss = runST $ do
    v <- V.thaw $ V.generate n succ
    forM_ ss $ uncurry (VM.swap v)
    V.unsafeFreeze v

parseLine :: String -> (Int,Int)
parseLine s = let (x,_:y) = break (== ',') s in (read x-1, read y-1)

main = do
    w <- readLn
    n <- readLn
    replicateM n getLine >>= V.mapM_ print . lot w . map parseLine
