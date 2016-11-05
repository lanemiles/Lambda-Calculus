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
    deriving Show

}
