module Main where
import Interp
import Parse
import System.IO
import System.Exit
import System.Environment


main :: IO ()
main = do
        args <- getArgs

        let cFlag = "-c" `elem` args || "-cn" `elem` args || "-nc" `elem` args
        let nFlag = "-n" `elem` args || "-cn" `elem` args || "-nc" `elem` args
        let fileArg = (hasFileArg args)
        programStr <- getProgramStr fileArg args
        let ast = parseInput programStr

        if cFlag
            then 
                if nFlag
                    then 
                        let freeVars = freeListify ast in
                            if length freeVars == 0
                                then
                                    case evalAstNum ast of
                                        Left x -> die (show x)
                                        Right x -> putStr (unlines (map show x))
                                else
                                    die ("Error: unbound variable(s) " ++ (take (length (prettyPrintUnbound freeVars) - 2) (prettyPrintUnbound freeVars)))
                    -- N FLAG
                    else
                        let freeVars = freeListify ast in
                            if length freeVars == 0
                                then
                                    case evalAst ast of
                                        Left x -> die (show x)
                                        Right x -> putStr (unlines (map show x))
                                else
                                    die ("Error: unbound variable(s) " ++ (take (length (prettyPrintUnbound freeVars) - 2) (prettyPrintUnbound freeVars)))
            else
                if nFlag
                    then
                        case evalAstNum ast of
                            Left x -> die (show x)
                            Right x -> putStr (unlines (map printNum x))
                    -- n flag
                    else 
                        case evalAst ast of
                            Left x -> die (show x)
                            Right x -> putStr (unlines (map show x))


prettyPrintUnbound :: [String] -> String
prettyPrintUnbound [] = ""
prettyPrintUnbound (x:xs) = x ++ ", " ++ prettyPrintUnbound xs



getProgramStr :: Bool -> [String] -> IO String
getProgramStr isFile args = if isFile
                                then readFile (filePath args)
                                else getContents

hasFileArg :: [String] -> Bool
hasFileArg [] = False
hasFileArg (x:xs) = if (take 1 x) /= "-" then True else hasFileArg xs

filePath :: [String] -> String
filePath [] = ""
filePath (x:xs) = if (take 1 x) /= "-" then x else filePath xs