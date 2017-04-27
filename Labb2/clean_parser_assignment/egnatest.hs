module CoreParser(Parser, char, return, fail, (#), (!), (?), (#>), (>->),
                  Parse, parse, toString, fromString) where

class Parse a where
    parse :: Parser a
    fromString :: String -> a
    fromString cs =
        case parse cs of
               Just(s, []) -> s
               Just(s, cs) -> error ("garbage '"++cs++"'")
               Nothing -> error "Nothing"
    toString :: a -> String


char :: Parser Char
char (c:cs) = Just(c,cs)
char [] = Nothing
