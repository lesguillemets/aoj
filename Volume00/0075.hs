import qualified Data.ByteString.Char8 as BC
main = BC.getContents >>=
    mapM_ (print . _id) . filter ((>=25) . bmi) . map readStudent . BC.lines

data Student = St { _id :: Int, _weight :: Double, _height :: Double}

readStudent :: BC.ByteString -> Student
readStudent = (\(n:w:h:_) -> St (read n) (read w) (read h)) .
    map BC.unpack . BC.split ','

bmi :: Student -> Double
bmi (St _ w h) = w / h^2
