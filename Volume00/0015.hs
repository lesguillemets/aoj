import Control.Monad
main = do
    n <- readLn
    replicateM_ n $ do
        x <- readLn :: IO Integer -- well, is this cheating?
        y <- readLn :: IO Integer
        let s = show $ x+y
        putStrLn $ if length s > 80
                        then "overflow"
                        else s
