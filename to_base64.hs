import System.Environment
import Data.Char
import Numeric
import Data.Bits

b64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

sumChars (t:ts) = ((ord t) `shiftL` (8 * (length ts))) + (sumChars ts)
sumChars [] = 0

--token must have a max lenght of 3 chars, if less than that, we add '=' chars
encodeToken token 0 = [b64!!(token .&. 63)]
encodeToken token x = [b64!!(((token .&. (63 `shiftL` (6 * x)))) `shiftR` (6 * x))] ++ (encodeToken token (x-1))

textToBase64 [] = []
textToBase64 t 
    | length t >= 3 = (encodeToken (sumChars (take 3 t)) 3) ++ (textToBase64 (drop 3 t))
    | length t == 2 = (take ((length t) + 1) (encodeToken ((sumChars t) `shiftL` (8 * (3-(length t)))) 3)) ++ "="
    | length t == 1 = (take ((length t) + 1) (encodeToken ((sumChars t) `shiftL` (8 * (3-(length t)))) 3)) ++ "=="
    | otherwise = ""

main = do
    input <- getLine
    putStrLn $ textToBase64 input