module Test where
import Interp
import Parse
import AST
import Test.HUnit
import qualified Data.Map as Map
import Data.Map (Map)

mainTest :: IO Counts
mainTest = do
    let testSimple = testLC "Simple!" "let x = lambda x. e; x" ["Error: UnboundVariable y"]
    let tests = TestList [TestLabel "test1" testSimple]
    runTestTT tests


testLC :: String -> String -> [String] -> Test
testLC info inp value = TestCase (assertEqual info value (print_me reduced_lcexp)) where
    print_me (Right x) = map (show) x
    print_me (Left x) = [show x]
    reduced_lcexp = evalAst res
    res = parseInput inp



    