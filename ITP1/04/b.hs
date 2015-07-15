import Text.Printf (printf)

s = printf "%.6f"

main = do
    r <- readLn
    putStr . s $ (pi::Double)*r*r
    putChar ' '
    putStrLn . s $ 2*pi*r
