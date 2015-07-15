import Data.List (intercalate)
showAsHMS :: Int -> String
showAsHMS n = let
    (m',s) = n `divMod` 60
    (h,m) = m' `divMod` 60
    in
        intercalate ":" $ map show [h,m,s]

main = getLine >>= putStrLn . showAsHMS . read
