{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (replicateM_)
import qualified Data.ByteString.Char8 as BC

main = replicateM_ 1000 $ BC.putStrLn "Hello World"
