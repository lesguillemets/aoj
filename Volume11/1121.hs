import Text.Parsec

digraphs :: [String]
digraphs = ["ld", "mb", "mp", "nc", "nd", "ng",
           "nt", "nw", "ps", "qu", "cw", "ts"]

-- {{{ Parser
kangChar :: Parsec String u String
kangChar = foldr1 (<|>) (map (try . string) digraphs)
        <|> (return <$> lower)

kangWord :: Parsec String u [String]
kangWord = many1 kangChar

kanglish :: Parsec String u [[String]]
kanglish = many1 kangChar `sepBy` spaces
-- }}}

-- {{{analiser
pairs :: [[String]] -> [(String,String)]
pairs = concatMap (\w -> zip w (tail w))
-- }}}

main = do
    parseTest kanglish "ncw"
    parseTest (pairs <$> kanglish) "nai tiruvantel ar varyuvantel i valar tielyama nu vilya"
-- vim:fdm=marker
