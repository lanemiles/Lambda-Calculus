{
module Grammar where
import Tokens
import AST
}

%name parseLambda
%tokentype { Token }
%error { parseError }

%token
    let { TokenLet }
    letrec { TokenLetRec }
    lambda  { TokenLambda }
    varName { TokenVarName $$ }
    '=' { TokenEq }
    '(' { TokenLParen }
    ')' { TokenRParen }
    '.' { TokenPeriod }
    ';' { TokenSemicolon }
    ':' { TokenColon }
    ',' { TokenComma }
    if { TokenIf }
    then { TokenThen }
    else { TokenElse }
    in { TokenIn }
    true { TokenTrue }
    false { TokenFalse }
    '-' { TokenMinus }
    not { TokenNot }
    fst { TokenFst }
    snd { TokenSnd }
    '+' { TokenPlus }
    '*' { TokenTimes }
    '/' { TokenDiv }
    and { TokenAnd }
    or { TokenOr }
    "==" { TokenDblEq }
    num { TokenNum $$ }

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
      num { Num $1 }
    | LCExp LCExpNoApp { App $1 $2 }
    | LCExpNoApp { $1 }

{

parseError :: [Token] -> a
parseError x = error "Parse error!"




}
