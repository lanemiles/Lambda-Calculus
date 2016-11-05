module Main where
import Interp
import Parse


main :: IO ()
main = do
    s <- getContents
    let ast = parseInput s
    let res = evalAst ast
    print res


