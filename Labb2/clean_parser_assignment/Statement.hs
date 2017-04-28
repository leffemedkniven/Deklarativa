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
    Write Expr.T
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
buildWhile (e, v) = While e v

readd = accept "read" -# word #- require ";" >-> buildRead
buildRead e = Read e

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e


exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (Assignment str expr: stmts) dict input =
    exec stmts (Dictionary.insert(str, (Expr.value expr dict)) dict) input
exec (If cond thenStmts elseStmts : stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Skip :stmts) dict input =
    exec stmts dict input
exec (While cond doStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (doStmts: While cond doStmts: stmts) dict input
    else exec stmts dict input

instance Parse Statement where
  parse = assignment ! iff ! skip ! begin ! while ! readd ! write
  toString = error "no"
