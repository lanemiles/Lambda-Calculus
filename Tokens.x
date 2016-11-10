{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  let                           { \s -> TokenLet }
  lambda                        { \s -> TokenLambda }
  \=                            { \s -> TokenEq }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  \.                            { \s -> TokenPeriod }
  \;                            { \s -> TokenSemicolon }
  $alpha [$alpha $digit \']*    { \s -> TokenVarName s }
  "let rec"                     { \s -> TokenLetRec }
  \:                            { \s -> TokenColon }
  \,                            { \s -> TokenComma }
  if                            { \s -> TokenIf }
  then                          { \s -> TokenThen }
  else                          { \s -> TokenElse }
  in                            { \s -> TokenIn }
  true                          { \s -> TokenTrue }
  false                         { \s -> TokenFalse }
  \-                            { \s -> TokenMinus }
  not                           { \s -> TokenNot }
  fst                           { \s -> TokenFst }
  snd                           { \s -> TokenSnd }
  \+                            { \s -> TokenPlus }
  \*                            { \s -> TokenTimes }
  \/                            { \s -> TokenDiv }
  and                           { \s -> TokenAnd }
  or                            { \s -> TokenOr }
  \=\=                          { \s -> TokenDblEq }
  $digit+                       { \s -> TokenNum (read s) }


{

-- The token type:

data Token = TokenLet
           | TokenLambda
           | TokenEq
           | TokenLParen
           | TokenRParen
           | TokenPeriod
           | TokenSemicolon
           | TokenVarName String
           | TokenLetRec
           | TokenColon
           | TokenComma
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenIn
           | TokenTrue
           | TokenFalse
           | TokenMinus
           | TokenNot
           | TokenFst
           | TokenSnd
           | TokenPlus
           | TokenTimes
           | TokenDiv
           | TokenAnd
           | TokenOr
           | TokenDblEq
           | TokenNum Int
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
