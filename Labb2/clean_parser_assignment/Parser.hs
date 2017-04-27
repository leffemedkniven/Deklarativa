module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #-

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return []

cons :: (a, [a]) -> [a]
cons(a, b) = a:b

(-#) :: Parser a -> Parser b -> Parser b --Self-made
m -# n = (m # n) >-> snd

(#-) :: Parser a -> Parser b -> Parser a --Self-made
m #- n = (m # n) >-> fst

spaces :: Parser String --Self-made
spaces = iter (char ? isSpace)

token :: Parser a -> Parser a
token m = m #- spaces

letter :: Parser Char --Self-made
letter =  char ? isAlpha

word :: Parser String
word = token (letter # iter letter >-> cons)

chars :: Int -> Parser String --Self-made
chars 0 = return []
chars n = char # chars (n-1) >-> cons

accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

-- require :: String -> Parser String --Self-made
-- require w s
--         | w == take (length w) s = accept w s
--         | otherwise = error ("Missing word " ++ w ++ " in string")

require :: String -> Parser String --Self-made
require w = (token (chars (length w)) ? (==w)) ! error ("Missing word " ++ w ++ " in string")


lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char
digit = char ? isDigit

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')
