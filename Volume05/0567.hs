import Control.Applicative
import Data.List

main = do
    _ <- getLine
    (a:b:_) <- map read . words <$> getLine
    c <- readLn
    toppings <- map read . lines <$> getContents
    print $ solve a b c toppings

solve :: Int -> Int -> Int -> [Int] -> Int
solve basePrice toppingPrice baseCalories toppings' =
    totalCalories `div` totalPrice
    where
        toppings = sortBy (flip compare) toppings'
        (totalPrice, totalCalories) =
            chooseToppings (basePrice,baseCalories) toppings
        chooseToppings p [] = p
        chooseToppings p@(currentPrice, currentCalories) (t:ts) =
            if currentCalories * toppingPrice < t * currentPrice
               then chooseToppings (
                   currentPrice + toppingPrice, currentCalories + t
                   ) ts
                else p

-- or maybe...
solve' basePrice toppingPrice baseCalories =
    maximum . map (uncurry div)
        . scanl' (\(cc,cp) t -> (t+cc,cp+toppingPrice)) (baseCalories,basePrice)
        . sortBy (flip compare)
