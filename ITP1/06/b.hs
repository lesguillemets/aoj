{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
data Suit = S | H | C | D deriving (Eq, Show, Read, Enum)

data Card = Card Suit Int

instance Enum Card where
    fromEnum (Card s i) = 13*fromEnum s + i
    toEnum n = let (s,i) = n `divMod` 13 in
        Card (toEnum s) i

instance Show Card where
    show (Card s i) = unwords [show s, show (i+1)]

fromStr :: String -> Card
fromStr s = let [c,n] = words s
                in
                    Card (read c) (read n -1)

mkList :: [Card] -> V.Vector Bool
mkList cs = runST $ do
    v <- VM.replicate 52 False
    forM_ cs $ \c -> VM.write v (fromEnum c) True
    V.unsafeFreeze v

cardsNotIn :: V.Vector Bool -> [Card]
cardsNotIn = map (toEnum . fst) . filter (not . snd) . zip [0..] . V.toList

main = do
    _ <- getLine
    getContents >>= mapM_ print . cardsNotIn . mkList . map fromStr . lines
