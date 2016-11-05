module Main where
import Interp
import Parse
import System.IO
import System.Environment


main :: IO ()
main = do
        args <- getArgs
        if (length args) == 1 && (head args) /= "-"
            then
                do
                    programStr <- readFile (head args)
                    let ast = parseInput programStr
                    let res = evalAst ast
                    print res
            else
                if (length args == 0) || (length args == 1 && (head args == "-"))
                    then 
                        do 
                            programStr <- getContents
                            let ast = parseInput programStr
                            let res = evalAst ast
                            print res
                    else
                        putStr "MEH"