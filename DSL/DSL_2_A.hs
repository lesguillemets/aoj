{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.ByteString.Char8 as BC
import Control.Monad
import Control.Monad.Primitive
import Control.Applicative
import Data.Maybe (fromJust)
import Prelude hiding (minimum)

data Query = Update Int Int
           | Find Int Int

initVal :: Int
initVal = 2^(31::Int) - 1

parseLn :: BC.ByteString -> Query
parseLn l = let
    (com:x':y':_) = BC.words l
    x = fst . fromJust . BC.readInt $ x'
    y = fst . fromJust . BC.readInt $ y'
    in
        case com of
            "0" -> Update x y
            "1" -> Find x y

solve :: Int -> [Query] -> IO ()
solve n qs = do
    v <- VM.replicate n initVal
    forM_ qs $ \q ->
        case q of
            (Find x y) -> let sl = VM.slice x y v
                              in minimum sl >>= print
            (Update k x) -> VM.write v k x

minimum :: (PrimMonad m, Ord a) => V.MVector (PrimState m) a -> m a
minimum = liftM V.minimum . V.freeze

main = do
    (n:q:_) <- map read . words <$> getLine
    replicateM q BC.getLine >>= solve n . map parseLn

