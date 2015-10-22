{-# LANGUAGE OverloadedStrings #-}

import Data.List (foldl')
import Control.Applicative ((<$>))
import Text.Parsec as P

{- Let's play with Parsec a bit. -}

type Input = String

data Term = N Int
          | O Operator

type Operator = (Int -> Int -> Int)

instance Show Term where
    show (N n) = "N " ++ show n
    show (O op) = "O " ++ case 1 `op` 1 of
                              2 -> "(+)"
                              0 -> "(-)"
                              1 -> "(*)"

fromChar :: Char -> Operator
fromChar '+' = (+)
fromChar '-' = (-)
fromChar '*' = (*)

expr :: Parsec Input u [Term]
expr = term `sepEndBy` spaces

term :: Parsec Input u Term
term = (N <$> int) <|> (O <$> operator)

operator :: Parsec Input u Operator
operator = fromChar <$> (char '+' <|> char '-' <|> char '*')

int :: Parsec Input u Int
int = read <$> many1 digit

-- unsafe !!!
calc :: [Term] -> Int
calc = head . foldl' f [] where
    f stack (N n) = n:stack
    f stack (O op) = let
        (x:y:ns) = stack
        in
            (y `op` x) : ns

main = do
    e <- parse expr "" <$> getLine
    case e of
        Right e' -> print . calc $ e'
        Left _ -> print "ERROR!"
