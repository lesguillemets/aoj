revPolRun :: [String] -> Double
revPolRun = iter [] where
    iter (n:_) [] = n
    iter (n:m:s) ("+":e) = iter ((m+n):s) e
    iter (n:m:s) ("-":e) = iter ((m-n):s) e
    iter (n:m:s) ("*":e) = iter ((m*n):s) e
    iter (n:m:s) ("/":e) = iter ((m/n):s) e
    iter stack (n:e) = iter (read n : stack) e

main = getContents >>= mapM_ (print . revPolRun . words) . lines
