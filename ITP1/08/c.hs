import qualified Data.Map.Strict as M
import Data.Char
import Data.List (foldl')

countChars :: String -> M.Map Char Int
countChars = foldl' (\acc c ->
                    if isAlpha c then flip (M.adjust succ) acc c
                                 else acc) initial
        where
            initial = M.fromAscList $ zip ['a'..'z'] (repeat 0)

showCount :: M.Map Char Int -> [String]
showCount = map (\(c,n) -> c:" : " ++ show n) . M.toAscList

main = getContents >>= mapM_ putStrLn . showCount . countChars . map toLower
