import Text.Parsec

ones :: Parsec String u Int
ones = length <$> many1 (char 'I')

main = do
    parseTest ones "I"
    parseTest ones "II"
    parseTest ones "III"
