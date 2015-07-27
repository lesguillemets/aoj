data WesternDate = WD Int Int Int deriving (Eq, Ord)
data JapaneseDate = JD Era Int Int Int
type Era = String

instance Show JapaneseDate where
    show (JD e y m d) = unwords (e: map show [y,m,d])

toJapanese :: WesternDate -> Maybe JapaneseDate
toJapanese date@(WD y m d)
    | date <= WD 1868 9 7     = Nothing
    | date <= WD 1912 7 29   = Just (JD "meiji" (y-1867) m d)
    | date <= WD 1926 12 24  = Just (JD "taisho" (y-1911) m d)
    | date <= WD 1989 1 7    = Just (JD "showa" (y-1925) m d)
    | otherwise              = Just (JD "heisei" (y-1988) m d)

readWD :: String -> WesternDate
readWD s = let (y:m:d:_) = map read . words $ s
               in WD y m d

answer :: Maybe JapaneseDate -> IO ()
answer Nothing = putStrLn "pre-meiji"
answer (Just d) = print d

main = getContents
    >>= mapM_ (answer .  toJapanese . readWD) . lines
