module AST where
import qualified Data.Map as Map
import Data.Map (Map)


data Error = UnboundVariable VarName
instance Show Error where
    show (UnboundVariable y) = "Error: unbound variable " ++ y

type Store = Map VarName LCExp


data Statement = Let VarName LCExp LCExp
         | LetRec VarName LCExp LCExp
         | LCExp LCExp
         deriving Show


type VarName = String


data LCExp = Var VarName
    | App LCExp LCExp
    | Lambda VarName LCExp
    | Cond LCExp LCExp LCExp
    | HasType LCExp Type
    | Num Int
    | True
    | False
    | Pair LCExp LCExp
    | Neg LCExp
    | Not LCExp
    | Fst LCExp
    | Snd LCExp
    | Plus LCExp LCExp
    | Minus LCExp LCExp
    | Mult LCExp LCExp
    | Div LCExp LCExp
    | And LCExp LCExp
    | Or LCExp LCExp
    | Eq LCExp LCExp
    deriving (Eq, Show)

data Type = Int
    | Bool
    | Func Type Type
    | Tuple Type Type
    deriving (Eq, Show)


-- instance Show LCExp where
--   show (Var x) = x
--   show (App (Var x) (Var y)) =  x ++ " " ++ y
--   show (App (Var x) e2) =  x ++ " (" ++ show e2 ++ ")"
--   show (App (Lambda x e) (Var y)) = "(" ++ show (Lambda x e) ++ ") " ++ y
--   show (App e1 (Var y)) = show e1 ++ " " ++ y
--   show (App e1 e2) = "(" ++ show e1 ++ ") " ++ show e2 
--   show (Lambda x e) = "lambda " ++ x ++ showLambda e

showLambda :: LCExp -> String
showLambda (Lambda x e) = " " ++ x ++ showLambda e
showLambda e = ". " ++ show e
