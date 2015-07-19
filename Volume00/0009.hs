import Control.Applicative
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

-- haskell can be this iterative!
sieve :: Int -> V.Vector Bool
sieve upto = runST $ do
    s <- VM.replicate (upto+1) True
    VM.write s 0 False
    VM.write s 1 False
    forM_ [2..(ceiling.sqrt.fromIntegral $ upto)] $ \i -> do
        isP <- VM.read s i
        when isP $ forM_ [i*i, i*i+i..upto] (\n -> VM.write s n False)
    V.unsafeFreeze s

primes :: Int -> V.Vector Int
primes = V.map fst . V.filter snd . V.imap (,) . sieve

main = do
    nums <- map read . lines <$> getContents
    let ps = primes (maximum nums)
    mapM_ (\i -> print . V.length $ V.takeWhile (<= i) ps) nums
