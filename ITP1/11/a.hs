import Data.List
mmul :: Num a => [[a]] -> [[a]] -> [[a]]
mmul a b' = let b = transpose b' in
    map (\l ->  map (sum . zipWith (*) l) b ) a

mul :: Num a => [[a]] -> [a] -> [a]
mul xs as = map (sum . zipWith (*) as) xs

rotX :: [[Int]]
rotY :: [[Int]]
rotZ :: [[Int]]
rotX = [[1,0,0],[0,0,-1],[0,1,0]]
rotY = [[0,0,1],[0,1,0],[-1,0,0]]
rotZ = [[0,-1,0],[1,0,0],[0,0,1]]



main = do
    print $ rotY `mmul` rotX
