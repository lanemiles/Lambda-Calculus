module AST where
import qualified Data.Map as Map
import Data.Map (Map)


data Error = UnboundVariable VarName | Empty | TypeError | RecError
instance Show Error where
    show (UnboundVariable y) = "Error: unbound variable " ++ y
    show Empty = "Error: empty program"
    show TypeError = "Error: mismatched types"
    show RecError = "Error: recursion"

type Store = Map VarName LCExp

type Context = Map VarName Type

data Statement = LetS VarName LCExp 
         | LetRecS VarName Type LCExp
         | LCExp LCExp
         deriving Show


type VarName = String


data LCExp = Var VarName
    | App LCExp LCExp
    | Lambda VarName Type LCExp
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
    | Let VarName LCExp LCExp
    | LetRec VarName Type LCExp LCExp
    deriving (Eq)

data Type = Int
    | Bool
    | Func Type Type
    | Tuple Type Type
    deriving (Eq, Show)


instance Show LCExp where
  show (Num n) = show n
  show AST.True = "true"
  show AST.False = "false"
  show (Pair e1 e2) = "(" ++ show e1 ++ ", " ++ show e2 ++ ")"
  show _ = "<function>"
