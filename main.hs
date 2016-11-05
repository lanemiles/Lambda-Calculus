module Main where
import Grammar
import Tokens

main :: IO ()
main = do
    s <- getContents
    let ast = parseLambda (scanTokens s)
    print ast
    