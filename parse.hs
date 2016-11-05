module Parse where
import Tokens
import Grammar
import AST

parseInput :: String -> [Statement]
parseInput s = parseLambda (scanTokens s)