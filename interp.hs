module Interp where
import AST
import qualified Data.Map as Map
import Data.Map (Map)

evalAst :: [Statement] -> Either Error [LCExp]
evalAst ast = Right (filter (/= Empty)) <*> (eval (Map.empty :: Store) ast)

eval :: Store -> [Statement] -> Either Error [LCExp]
eval st [] = Right []
eval st ((Let x e):xs) = case evalLCExp st e of
        (Left err) -> (Left err)
        (Right e') -> (:) <$> (Right Empty) <*> (eval (Map.insert x e' st) xs)
eval st ((LCExp e):xs) = (:) <$> evalLCExp st e <*> eval st xs




evalLCExp :: Store -> LCExp -> Either Error LCExp
evalLCExp st (Var k) = case Map.lookup k st of
        Nothing -> Left (UnboundVariable k)
        (Just v) -> Right v
evalLCExp st (App e1 e2) =  case evalLCExp st e2 of
                                Left err -> Left err
                                (Right e2') -> case evalLCExp st e1 of
                                    Left err -> Left err
                                    Right (Lambda x e1') -> evalLCExp st (subst st e1' x e2')
evalLCExp st (Lambda x e) = Right (Lambda x e)



subst :: Store -> LCExp -> VarName -> LCExp -> LCExp
subst st (Var x) y e2 = if x == y then e2 else (Var x)
subst st (App e11 e12) x e2 = (App (subst st e11 x e2) (subst st e12 x e2))
subst st (Lambda x e1) y e2 = if x == y then (Lambda x e1) else (Lambda x (subst st e1 y e2))