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
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
