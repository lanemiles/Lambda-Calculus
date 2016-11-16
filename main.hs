module Main where
import Interp
import Parse
import System.IO
import System.Exit
import System.Environment


main :: IO ()
main = do
        args <- getArgs

        let uFlag = "-u" `elem` args
        let fileArg = (hasFileArg args)
        programStr <- getProgramStr fileArg args
        let ast = parseInput programStr
        
        if uFlag then 
            case evalAst ast of
                Left x -> die (show x)
                Right x -> putStr (unlines (map show x))
        else
            case evalTypesAst ast of
                Left x -> die (show x)
                _ -> case evalAst ast of
                        Left x -> die (show x)
                        Right x -> putStr (unlines (map show x))


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
