{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
import Control.Monad
import Data.Maybe (fromJust)
import qualified Data.Sequence as S
import Data.Sequence (ViewL(..), (|>), viewl)
import qualified Data.ByteString.Char8 as BC


type ProcessName = BC.ByteString

data Scheduler = Sch {
               _t :: Int, -- current time
               _q :: Int, -- quantum
               _que :: S.Seq (ProcessName, Int)
}

runScheduler :: Scheduler -> [(ProcessName, Int)]
runScheduler s@(Sch {..})
    | S.null _que = []
    | cPTime <= _q = (cPName, _t + cPTime)
                        : runScheduler (s{_que = re, _t = _t + cPTime})
    | otherwise = runScheduler (
            s{_que = re |> (cPName, cPTime- _q), _t = _t + _q}
            )
    where
        ((cPName, cPTime) :< re) = viewl _que

parseLine :: BC.ByteString -> (ProcessName,Int)
parseLine l = let (n:t:_) = BC.words l
                  in (n, fst . fromJust . BC.readInt $ t)

main = do
    (n:q:_) <- map read . words <$> getLine
    replicateM n (parseLine <$> BC.getLine) >>=
        mapM_ (\(s,t) -> BC.putStrLn . BC.unwords $ [s, BC.pack . show $ t])
        . runScheduler . Sch 0 q . S.fromList
