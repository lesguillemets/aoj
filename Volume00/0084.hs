import Text.Parsec
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC

separators = " ,."

words' :: Parsec BC.ByteString u [BLC.ByteString]
words' = (BLC.pack <$> many (noneOf separators)) `sepEndBy` oneOf separators

valid :: BLC.ByteString -> Bool
valid s = let l = BLC.length s in
    3 <= l && l <= 6

listWords :: BC.ByteString -> [BLC.ByteString]
listWords src = case parse words' "" src of
                    Left _ -> []
                    Right wrds -> filter valid wrds

main = BC.getLine >>= BLC.putStrLn . BLC.unwords . listWords
