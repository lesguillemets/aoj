import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromJust)
import Control.Monad (foldM_)

type IntSets = V.Vector Int

unite :: IntSets -> Int -> Int -> IntSets
unite v x y =
    let to = v ! x
        from = v ! y
        in
        V.map (\n -> if n == from then to else n) v

same :: IntSets -> Int -> Int -> Bool
same v x y = v!x == v!y

data Command = Same Int Int
            | Unite Int Int

fromBStr :: BC.ByteString -> Command
fromBStr bs =
    let (c:x:y:_) = map (fst . fromJust . BC.readInt) . BC.words $ bs
        in
        (case c of
             0 -> Unite
             1 -> Same) x y

getCommands :: IO [Command]
getCommands = map fromBStr . BC.lines <$> BC.getContents

talk :: IntSets -> [Command] -> IO ()
talk = foldM_ step

step :: IntSets -> Command -> IO IntSets
step v (Same x y) = do
    putStrLn $ if same v x y then "1" else "0"
    return v
step v (Unite x y) = return (unite v x y)


main = do
    (n:_) <- map read . words <$> getLine
    getCommands >>= talk (V.fromList [0..n-1])
