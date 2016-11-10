module Test where
-- import Interp
-- import Parse
-- import AST
-- import Test.HUnit
-- import qualified Data.Map as Map
-- import Data.Map (Map)

-- mainTest :: IO Counts
-- mainTest = do
--     let t1 = testLC "Simple!" "let zero = lambda s z. z;let succ = lambda n. lambda s z. s (n s z);succ (succ zero)" ["lambda s z. s ((lambda s z. s ((lambda s z. z) s z)) s z)"]
--     let t2 = testLC "Simple!" "let zero = lambda s z. z;let succ = lambda n. lambda s z. s (n s z);succ (succ zero);succ (succ (succ zero))" ["lambda s z. s ((lambda s z. s ((lambda s z. z) s z)) s z)", "lambda s z. s ((lambda s z. s ((lambda s z. s ((lambda s z. z) s z)) s z)) s z)"]
--     let t3 = testLC "Simple!" "(lambda x. y) (lambda x. x)" ["Error: unbound variable y"]
--     let tests = TestList [TestLabel "test1" t1, TestLabel "test2" t2, TestLabel "test3" t3]
--     runTestTT tests


-- testLC :: String -> String -> [String] -> Test
-- testLC info inp value = TestCase (assertEqual info value (print_me reduced_lcexp)) where
--     print_me (Right x) = map (show) x
--     print_me (Left x) = [show x]
--     reduced_lcexp = evalAst res
--     res = parseInput inp



    
