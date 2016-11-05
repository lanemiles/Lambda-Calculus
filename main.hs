module Main where
import Grammar
import Tokens
import qualified Data.Map as Map
import Data.Map (Map)

main :: IO ()
main = do
    s <- getContents
    let ast = parseLambda (scanTokens s)
    let reduced_lcexp = eval ast
    print reduced_lcexp


data Error = UnboundVariable VarName deriving (Show)
data Store = Map VarName LCExp

eval :: Store -> [Statement] -> [LCExp]
eval st [] = []
eval st ((Let x e):xs) = undefined : (eval (Map.insert x (evalLCExp st e) st) xs)
eval st ((LCExp e):xs) = evalLCExp st e : eval st xs


evalLCExp :: Store -> LCExp -> Either Error LCExp
evalLCExp st (Var k) = case Map.lookup k st of
        Nothing -> Left (UnboundVariable k)
        (Just v) -> Right v
evalLCExp st (App e1 e2) =  evalLCExp st (subst st e1' x e2') where
								(Lambda x e1') = evalLCExp st e1
								e2' = evalLCExp st e2
evalLCExp st (Lambda x e) = (Lambda x e)


subst :: Store -> LCExp -> VarName -> LCExp -> LCExp
subst st (Var x) y e2 = if x == y then e2 else (Var x)
subst st (App e11 e12) x e2 = (App (subst st e11 x e2) (subst st e12 x e2))
subst st (Lambda x e1) y e2 = if x == y then (Lambda x e1) else (Lambda x (subst st e1 y e2))