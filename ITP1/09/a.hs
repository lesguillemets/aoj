import Data.Char (toLower)
import Control.Applicative
main = do
    w <- map toLower <$> getLine
    getContents >>=
        print. length. filter (== w). takeWhile (/= "END_OF_TEXT"). words. map toLower
