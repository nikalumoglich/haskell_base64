import System.Environment
import Data.Char
import Numeric
import Data.Bits
import Data.List

b64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

sumChars [] = 0
sumChars "=" = 0
sumChars "==" = 0
sumChars (t:ts) = case elemIndex t b64 of
    Just x -> (x `shiftL` (6 * (length ts))) + (sumChars ts)
    Nothing -> 0

decodeToken token 0 = [chr (token .&. 255)]
decodeToken token x = [chr ((token .&. (255 `shiftL` (8 * x))) `shiftR` (8 * x))] ++ (decodeToken token (x-1))

base64ToText [] = []
base64ToText t = (decodeToken (sumChars (take 4 t)) 2) ++ (base64ToText (drop 4 t))

main = do
    input <- getLine
    putStrLn $ base64ToText input