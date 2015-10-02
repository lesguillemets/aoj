import Control.Arrow
import Data.Array.Unboxed as U
import Data.List
import Data.Maybe (fromJust)
import Control.Monad

size = 3 :: Int

type Field = U.UArray Loc Int
type Loc = (Int,Int)

data Dir = Up | Rt | Dw | Lf deriving (Show, Enum, Eq)
toVect :: Dir -> (Int,Int)
toVect Up = (-1,0)
toVect Rt = (0,1)
toVect Dw = (1,0)
toVect Lf = (0,-1)

data Puzzle = Puzzle {
            _field :: Field,
            _zloc :: Loc
}

fromList :: [Int] -> Puzzle
fromList xs = let
    f = listArray ((0,0),(size-1,size-1)) xs
    in
        Puzzle {
               _field = f,
               _zloc = (`divMod` size) .  head  $ 0 `elemIndices` xs
               }


instance Show Puzzle where
    show = showTable . _field

showTable :: Field -> String
showTable = unlines . map (tail . concatMap ('|':)) . cutEvery size
    . map show . elems

cutEvery :: Int -> [a] -> [[a]]
cutEvery _ [] = []
cutEvery n xs = let (pre,post) = splitAt n xs
                    in pre : cutEvery n post

movableDirs :: Puzzle -> [Dir]
movableDirs p = let
    (y,x) = _zloc p
    in
        filter (
               case x of
                   0 -> (/= Lf)
                   2 -> (/= Rt)
                   _ -> const True
              )
        . filter (
                 case y of
                    0 -> (/= Up)
                    2 -> (/= Dw)
                    _ -> const True
                ) $ enumFrom Up

move :: Puzzle -> Dir -> Puzzle
move p d = let
    next = toVect d `tupAdd` _zloc p
    nexp = _field p // [(next,0), (_zloc p, _field p ! next)]
    in
        p {
          _field = nexp,
          _zloc = next
        }

tupAdd :: (Num a, Num b) => (a,b) -> (a,b) -> (a,b)
tupAdd (x,y) = (+x) *** (+y)

nextPossibles :: Puzzle -> [Puzzle]
nextPossibles p = map (move p) (movableDirs p)

isComplete :: Puzzle -> Bool
isComplete = (== [1,2,3,4,5,6,7,8,0]) . elems . _field

data Phase = Phase {_puz :: Puzzle, _actualCost :: Int}
instance Show Phase where
    show p = unlines ["Phase :", show (_puz p), "cost" ++ show (_actualCost p)]
nextPhases :: Phase -> [Phase]
nextPhases ph = let
    p = _puz ph
    c = _actualCost ph in
        map (\pz -> Phase pz (c+1)) . nextPossibles $ p

diffsNum :: Puzzle -> Int
diffsNum = length . filter (\(x,y)-> y /= 0  && x /= y)
    . zip [1,2,3,4,5,6,7,8,0] . elems . _field

manHattan :: Puzzle -> Int
manHattan = sum . map (\((x0,y0),e) -> let (x1,y1) = (e-1) `divMod` 3
                                   in
                                      abs (x1-x0) + abs (y1-y0))
                                      . filter ((/=0) . snd) . assocs . _field

type Estimator = Puzzle -> Int


idaStar :: Estimator -> Puzzle -> Maybe (Phase, Int)
idaStar f p = let
    iter :: Int -> Int -> [Phase] -> Maybe (Phase,Int)
    iter steps depth [] = iter steps (depth+1) [Phase p 0]
    iter steps depth ss =
        case find (isComplete . _puz) next of
            Just pu -> Just (pu,steps)
            Nothing -> iter (steps+1) depth next
        where
            next = filter ((<= depth) . cost f) . concatMap nextPhases $ ss
    in
        iter 0 1 [Phase p 0]

cost :: Estimator -> Phase -> Int
cost f p = _actualCost p + f (_puz p)

main = replicateM 3 getLine >>=
        print . _actualCost . fst . fromJust . idaStar manHattan . fromList
            . map read . words . unlines
