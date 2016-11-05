{
module Grammar where
import Tokens
}

%name parseLambda
%tokentype { Token }
%error { parseError }

%token
    let { TokenLet }
    lambda  { TokenLambda }
    varName { TokenVarName $$ }
    '=' { TokenEq }
    '(' { TokenLParen }
    ')' { TokenRParen }
    '.' { TokenPeriod }
    ';' { TokenSemicolon }

%%

Program : 
        Program ';' Statement          { $1 ++ [$3] }
      | Program ';'                 { $1 }
      | Statement            { [$1] }
      | {- empty -}     { [] }

Statement : 
      let varName '=' LCExp { Let $2 $4 }
    | LCExp { LCExp $1 }

LCExpNoApp :
      '(' LCExp ')' { $2 }
    | varName { Var $1 } 
    | lambda varName LambdaNoLambda {Lambda $2 $3 }

LambdaNoLambda : 
    '.' LCExp { $2 }
    | varName LambdaNoLambda { Lambda $1 $2 } 

LCExp :
      LCExp LCExpNoApp { App $1 $2 }
    | LCExpNoApp { $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

type VarName = String

data Statement = Let VarName LCExp
         | LCExp LCExp
         deriving Show

data LCExp = Var VarName
    | App LCExp LCExp
    | Lambda VarName LCExp
    | Empty
    deriving (Eq)


instance Show LCExp where
  show (Var x) = x
  show (App (Var x) (Var y)) =  x ++ " " ++ y
  show (App (Var x) e2) =  x ++ " (" ++ show e2 ++ ")"
  show (App e1 (Var y)) = "(" ++ show e1 ++ ") " ++ y
  show (App e1 e2) = "(" ++ show e1 ++ ") (" ++ show e2 ++ ")"
  show (Lambda x e) = "lambda " ++ x ++ " " ++ showLambda e

showLambda :: LCExp -> String
showLambda (Lambda x e) = x ++ " " ++ showLambda e
showLambda e = ". " ++ show e


}
