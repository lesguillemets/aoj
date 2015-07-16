import Control.Monad
import qualified Data.Array.Unboxed as A
import qualified Data.Array.ST as AS
import Data.List (intercalate)

type Info = (Int,Int,Int,Int)

type Room = (Int,Int,Int)

towers = 4
floors = 3
rooms = 10

-- let's play with ST.
manage :: [Info] -> A.UArray Room Int
manage is = AS.runSTUArray $ do
    rec <- AS.newArray ((0,0,0),(towers-1,floors-1,rooms-1)) 0
    forM_ is $
        \(tow,fl,r,n) -> let
            loc = (tow,fl,r)
            in
                AS.readArray rec loc >>= AS.writeArray rec loc . (+n)
    return rec

readline :: String -> Info
readline = (\[x,y,z,w] -> (x,y,z,w+1)) . map (subtract 1 . read) . words

-- let's see how iterative haskell can be!
showRooms :: A.UArray Room Int -> [String]
showRooms ls = let dats = takeEvery (floors*rooms) . map show $ A.elems ls in
    intercalate [replicate 20 '#'] . map (map unwords . takeEvery rooms) $ dats

        
takeEvery :: Int -> [a] -> [[a]]
takeEvery _ [] = []
takeEvery n xs = let (pre,post) = splitAt n xs
                     in pre:takeEvery n post

main = do
    _ <- getLine
    getContents >>= mapM_ putStrLn . showRooms . manage . map readline . lines
