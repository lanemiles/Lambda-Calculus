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
    int { TokenInt }
    bool { TokenBool }
    "->" { TokenArr }

%right "=="
%right in
%left '+' '-'
%left '*' '/'
%left NEG not fst snd

%%

Program : 
        Program ';' Statement          { $1 ++ [$3] }
      | Program ';'                 { $1 }
      | Statement            { [$1] }
      | {- empty -}     { [] }

Statement : 
      let varName '=' LCExp { LetS $2 $4 }
    | let varName ':' Type '=' LCExp{ LetS $2 (HasType $6 $4) }
    | letrec varName ':' Type '=' LCExp { LetRecS $2 $4 $6 }
    | LCExp { LCExp $1 }

LCExpNoApp :
      '(' LCExp ')' { $2 }
    | varName { Var $1 } 
    | lambda varName ':' Type '.' LCExp { Lambda $2 $4 $6 }
    | lambda '(' varName ':' Type ')' LambdaNoLambda { Lambda $3 $5 $7 }

LambdaNoLambda : 
    '.' LCExp { $2 }
    | '(' varName ':' Type ')' LambdaNoLambda { Lambda $2 $4 $6 } 

LCExp :
      num { Num $1 }
    | true { AST.True }
    | false { AST.False }
    | '(' LCExp ',' LCExp ')' { Pair $2 $4 }
    | if LCExp then LCExp else LCExp { Cond $2 $4 $6 }
    | '(' LCExp ':' Type ')' { HasType $2 $4 }
    | LCExp LCExpNoApp { App $1 $2 }
    | LCExpNoApp { $1 }
    | '-' LCExp %prec NEG { Neg $2 }
    | not LCExp { Not $2 }
    | fst LCExp { Fst $2 }
    | snd LCExp { Snd $2 }
    | LCExp '+' LCExp { Plus $1 $3 }
    | LCExp '-' LCExp { Minus $1 $3 }
    | LCExp '*' LCExp { Mult $1 $3 }
    | LCExp '/' LCExp { Div $1 $3 }
    | LCExp and LCExp { And $1 $3 }
    | LCExp or LCExp { Or $1 $3 }
    | LCExp "==" LCExp { Eq $1 $3 }
    | let varName '=' LCExp in LCExp { Let $2 $4 $6 }
    | let varName ':' Type '=' LCExp in LCExp { Let $2 (HasType $6 $4) $8 }
    | letrec varName ':' Type '='LCExp in LCExp { LetRec $2 $4 $6 $8 }

Type :
      int { Int }
    | bool { Bool }
    | Type "->" Type { Func $1 $3 }
    | '(' Type ',' Type ')' { Tuple $2 $4 }

{

parseError :: [Token] -> a
parseError x = error "Parse error!"




}
