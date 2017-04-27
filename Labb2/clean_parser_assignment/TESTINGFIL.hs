
---MY TEST SHIT
module Statement(T, parse, toString, fromString) where
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

test1 = (accept "skip" # require ";") "skip;" {-  Just ("count","") -}
test2 = (accept "begin" -# iter parse #- require "end") "begin x:=0; x:=x+1; end"

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

instance Parse Statement where
  parse = assignment
  toString = error "no"
