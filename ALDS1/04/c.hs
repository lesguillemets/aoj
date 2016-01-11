{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)
import qualified Data.Set as S
-- Should I implement this myself?
import Control.Monad

data Command = Insert Int
             | Find Int

fromString :: ByteString -> Command
fromString s = let (cmd:str:_) = BC.words s
                   n = fromStr str
                   in
                   case cmd of
                        "insert" -> Insert n
                        "find" -> Find n

fromStr :: ByteString -> Int
fromStr = BC.foldl' (\acc c -> 5*acc + case c of
                                        'A' -> 1
                                        'T' -> 2
                                        'G' -> 3
                                        'C' -> 4) 0

follow :: S.Set Int -> [Command] -> IO ()
follow s [] = return ()
follow s (c:cs) = case c of
                       Insert str -> follow (S.insert str s) cs
                       Find str -> do
                           putStrLn $ if str `S.member` s
                                         then "yes"
                                         else "no"
                           follow s cs

main = do
    n <- readLn
    replicateM n BC.getLine >>= follow S.empty . map fromString
