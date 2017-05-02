module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip |
    Begin [Statement] |
    While Expr.T Statement |
    Read String |
    Write Expr.T |
    Repeat Statement Expr.T
    deriving Show


assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

iff = accept "if" -# (Expr.parse #- require "then" # parse)
                                        #- require "else" # parse >-> buildIff
buildIff ((e, s), s') = If e s s'

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip _ = Skip

begin = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin s = Begin s

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e, s) = While e s

readd = accept "read" -# word #- require ";" >-> buildRead
buildRead v = Read v

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e

repeatt = accept "repeat" -# parse # require "until" -# Expr.parse #- require ";" >-> buildRepeat
buildRepeat (s, e) = Repeat s e


exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] dict input = []
exec (Assignment v e: stmts) dict input =
    exec stmts (Dictionary.insert(v, (Expr.value e dict)) dict) input
exec (If cond thenStmts elseStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Skip : stmts) dict input =
    exec stmts dict input
exec (Begin (x:xs): stmts) dict input =
    exec (x:xs++stmts) dict input
exec (While cond doStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (doStmts: While cond doStmts: stmts) dict input
    else exec stmts dict input
exec (Read v: stmts) dict (x:xs) =
    exec stmts (Dictionary.insert(v, x) dict) xs
exec (Write e: stmts) dict input =
    (Expr.value e dict) :(exec stmts dict input)
exec (Repeat repeatStmt untilStmt: stmts) dict input =
      if (Expr.value untilStmt dict)>0
      then exec stmts dict input
      else exec (repeatStmt: Repeat repeatStmt untilStmt: stmts) dict input

instance Parse Statement where
  parse = assignment ! iff ! skip ! begin ! while ! readd ! write ! repeatt
  toString = toStr

toStr (Assignment v e) = v ++ ":=" ++ toString e ++ ";\n"
toStr (If e s s') = "if " ++ toString e ++ " then\n" ++ toString s
                                              ++ "else\n " ++ toString s' ++ "\n"
toStr (Skip) = "skip:\n"
-- toStr (Begin s) = "begin" ++
toStr (While e s) = "while " ++ toString e ++ " do\n " ++ toString s ++ "\n"
toStr (Read v) = "read " ++ v ++ ";\n"
toStr (Write e) = "write " ++ toString e ++ ";"
toStr (Begin s) = "Begin\n " ++ listToString s ++ " end"

listToString :: [T] -> String
listToString [] = []
listToString (stmt : s) = toString stmt ++ listToString s
