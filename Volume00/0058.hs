import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
type Vect = (Double, Double)

threshold :: Double
threshold = 0.0000001

isOrthog :: Vect -> Vect -> Bool
isOrthog (x0,y0) (x1,y1) = abs (x0*x1+y0*y1) < threshold

parseLn :: [Double] -> Maybe (Vect, Vect)
parseLn [xa,ya,xb,yb,xc,yc,xd,yd] = Just ((xb-xa,yb-ya),(xd-xc,yd-yc))
parseLn _ = error "invalid imput!"

addLeadingZeros :: String -> String
addLeadingZeros s = if head s == '.' then '0':s else s

removeEmpty :: String -> String
removeEmpty = filter (/= ' ')

main = getContents >>=
    mapM_ (\l -> case parseLn . catMaybes . map (readMaybe . addLeadingZeros) .  words $ l of
            Nothing -> return ()
            Just c -> putStrLn . (\b -> if b then "YES" else "NO")
                    . uncurry isOrthog $ c
                    ) . lines
