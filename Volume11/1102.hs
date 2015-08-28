{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
import Text.Parsec
import Control.Applicative (liftA, liftA2, (<$>), (<*>), (*>), (<*))
import Data.Complex
import Data.List
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BLC

type Comp = Complex Integer

-- {{{ Parsing
number :: Parsec ByteString u (Maybe Comp)
number = validate . (:+0) . read <$> many1 digit
    <|>
        const (Just (0:+1)) <$> char 'i'

unit :: Parsec ByteString u (Maybe Comp)
unit = char '(' *> calc <* char ')' <|> number

term :: Parsec ByteString u (Maybe Comp)
term = foldl1' (*:) <$> unit `sepBy1` char '*'

calc :: Parsec ByteString u (Maybe Comp)
calc = do
    t0 <- term
    fs <- many $
            (+:) <$> (char '+' *> term)
        <|>
            flip (-:) <$> (char '-' *> term)
    return $ foldl' (\acc f -> f acc) t0 fs
-- }}}

-- {{{ Calculation
neg :: Comp -> Comp
neg (a:+b) = negate a :+ negate b
add :: Comp -> Comp -> Comp
add (a:+b) (c:+d) = (a+c) :+ (b+d)

mul :: Comp -> Comp -> Comp
mul (a:+b) (c:+d) = (a*c-b*d) :+ (b*c+d*a)

sub :: Comp -> Comp -> Comp
sub z w = add z (neg w)

threshold :: Integer
threshold = 10000

validate :: Comp -> Maybe Comp
validate (c@(a :+ b)) = if abs a <= threshold && abs b <= threshold
                            then Just c
                            else Nothing

(+:) :: Maybe Comp -> Maybe Comp -> Maybe Comp
(+:) z w = validate =<< liftA2 add z w
(*:) :: Maybe Comp -> Maybe Comp -> Maybe Comp
(*:) z w = validate =<< liftA2 mul z w
(-:) :: Maybe Comp -> Maybe Comp -> Maybe Comp
(-:) z w =  validate =<< liftA2 sub z w
-- }}}

-- {{{ output
answer :: Maybe Comp -> BLC.ByteString
answer Nothing = "overflow"
answer (Just (0:+0)) = "0"
answer (Just (0:+b)) = BLC.pack $ show b ++ "i"
answer (Just (a:+b)) = BLC.pack $ case signum b of
                           1 -> show a ++ "+" ++ show b ++ "i"
                           -1 -> show a ++ show b ++ "i"
                           0 -> show a
unright :: Either ParseError (Maybe Comp) -> Maybe Comp
unright (Right r) = r
-- }}}

main = BC.getContents >>=
        mapM_ (BLC.putStrLn . answer . unright . parse calc "") . BC.lines
-- vim:foldmethod=marker
