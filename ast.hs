module AST where
import qualified Data.Map as Map
import Data.Map (Map)


data Error = UnboundVariable VarName | Empty | TypeError
instance Show Error where
    show (UnboundVariable y) = "Error: unbound variable " ++ y
    show Empty = "Error: empty program"
    show TypeError = "Error: mismatched types"

type Store = Map VarName LCExp


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
    deriving (Eq, Show)

data Type = Int
    | Bool
    | Func Type Type
    | Tuple Type Type
    deriving (Eq, Show)
