{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)
import qualified Data.Set as S
-- Should I implement this myself?
import Control.Monad

data Command = Insert ByteString
             | Find ByteString

fromString :: ByteString -> Command
fromString s = let (cmd:str:_) = BC.words s
                   in
                   case cmd of
                        "insert" -> Insert str
                        "find" -> Find str

follow :: S.Set ByteString -> [Command] -> IO ()
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
    replicateM n BC.getLine >>= follow (S.empty) . map fromString

