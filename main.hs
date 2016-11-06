module Main where
import Interp
import Parse
import System.IO
import System.Environment


main :: IO ()
main = do
        args <- getArgs

        let cFlag = "-c" `elem` args
        let nFlag = "-n" `elem` args

        -- let fileArg = (hasFileArg args)

        programStr <- getContents
        let ast = parseInput programStr
        let res = evalAst ast
        print res

--         if cFlag
--             then if fileArg
--                 then 
--                     do 
--                         programStr <- readFile (filePath args)
--                         let ast = parseInput programStr
--                         let res = evalAst ast
--                         print res
--                 else
--                     do
--                         programStr <- getContents
--                         let ast = parseInput programStr
--                         let res = evalAst ast
--                         print res
--             else
--                 if fileArg
--                     then 
--                         do 
--                             programStr <- readFile (filePath args)
--                             let ast = parseInput programStr
--                             let res = evalAst ast
--                             print res
--                     else
--                         do
--                             programStr <- getContents
--                             let ast = parseInput programStr
--                             let res = evalAst ast
--                             print res




-- hasFileArg :: [String] -> Bool
-- hasFileArg [] = False
-- hasFileArg (x:xs) = if (take 1 x) /= "-" then True else hasFileArg xs

-- filePath :: [String] -> String
-- filePath [] = ""
-- filePath (x:xs) = if (take 1 x) /= "-" then x else filePath xs