import Text.Parsec

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

main = readFile "./1104_check.txt" >>= parseTest inputs
-- vim:fdm=marker
