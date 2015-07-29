{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Char8 (ByteString)
pTa :: ByteString -> ByteString
pTa = BC.intercalate " " . map (
        \c -> case c of
                    "apple" -> "peach"
                    "peach" -> "apple"
                    s -> s
                    ) . BC.split ' '

main = BC.getLine >>= BC.putStrLn . pTa
