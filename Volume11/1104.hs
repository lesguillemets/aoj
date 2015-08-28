import Text.Parsec
import Data.List
-- learning parsec..
-- {{{ data
data Command = Forward Int
             | Backward Int
             | TurnRight
             | TurnLeft deriving (Show)

type Loc = (Int, Int)
type Vec = (Int, Int)

turnRight :: Vec -> Vec
turnRight (x,y) = (y,-x)
turnLeft :: Vec -> Vec
turnLeft (x,y) = (-y,x)

data Robot = Robot {
           _loc :: Loc,
           _dir :: Vec,
           _field :: (Int,Int)
}
instance Show Robot where
    show (Robot (x,y) _ _) = unwords. map show $ [x,y]
initRobot :: Int -> Int -> Robot
initRobot x y = Robot (1,1) (0,1) (x,y)

(*:) :: Int -> Vec -> Vec
n *: (x,y) = (n*x, n*y)
(+:) :: Vec -> Vec -> Vec
(x,y) +: (z,w) = (x+z,y+w)
fitIn :: Int -> Int -> Int
fitIn x w = if x <= 0 then 1 else min x w
stayInside :: Vec -> Vec -> Vec
stayInside (x,y) (w,h) = (x `fitIn` w, y `fitIn` h)

data Input = Input Int Int [Command] deriving (Show)
-- }}}
-- {{{ parser
num :: Parsec String u Int
num = read <$> many1 digit

command :: Parsec String u Command
command = Forward <$> (string "FORWARD" *> spaces *> num)
      <|> Backward <$> (string "BACKWARD" *> spaces *> num)
      <|> const TurnRight <$> string "RIGHT"
      <|> const TurnLeft <$> string "LEFT"

singleInput :: Parsec String u Input
singleInput = Input <$> (spaces *> num)
                    <*> (spaces *> num <* spaces)
                    <*> command `sepEndBy` spaces
                    <* string "STOP"
inputs :: Parsec String u [Input]
inputs = (singleInput <* spaces) `manyTill` (char '0' *> spaces *> char '0')
-- }}}
-- {{{ solver
applyCmd :: Robot -> Command -> Robot
applyCmd r@(Robot l d s) (Forward n) =
        r {_loc = (l +: (n *: d)) `stayInside` s}
applyCmd r@(Robot l d s) (Backward n) =
        r {_loc = (l +: ((-n) *: d)) `stayInside` s}
applyCmd r@(Robot{_dir=d}) TurnRight = r {_dir = turnRight d}
applyCmd r@(Robot{_dir=d}) TurnLeft = r {_dir = turnLeft d}

solve :: Input -> Robot
solve (Input x y cmds) = let r = initRobot x y in
    foldl' applyCmd r cmds
-- }}}
main = getContents >>=
    mapM_ (print . solve) . (\(Right r) -> r) . parse inputs ""
-- vim:fdm=marker
