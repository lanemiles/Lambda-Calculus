module Interp where
import AST
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

evalAst :: [Statement] -> Either Error [LCExp]
evalAst p = if null p then Left Empty else eval Map.empty p

eval :: Store -> [Statement] -> Either Error [LCExp]
eval st [] = Right []
eval st ((Let x e):xs) = case evalLCExp st e of
        (Left err) -> (Left err)
        (Right e') -> eval (Map.insert x e' st) xs
eval st ((LCExp e):xs) = (:) <$> evalLCExp st e <*> eval st xs

evalLCExp :: Store -> LCExp -> Either Error LCExp
evalLCExp st (Var k) = case Map.lookup k st of
        Nothing -> Left (UnboundVariable k)
        (Just v) -> Right v
evalLCExp st (App e1 e2) =  case evalLCExp st e1 of
                                Left err -> Left err
                                Right (Lambda x e1') -> case evalLCExp st e2 of
                                    Left err -> Left err
                                    Right e2' -> evalLCExp st (subst st e1' x e2')
evalLCExp st (Lambda x e) = Right (Lambda x e)
evalLCExp st (Cond e1 e2 e3) = case evalLCExp st e1 of
                                    Right True -> evalLCExp st e2
                                    Right False -> evalLCExp st e3
                                    _ -> Left TypeError
evalLCExp st (HasType e _) = evalLCExp st e
evalLCExp _ (Num n) = Right (Num n)
evalLCExp _ True = Right True
evalLCExp _ False = Right False
evalLCExp st (Pair e1 e2) = Pair (evalLCExp st e1) (evalLCExp st e2)
evalLCExp st (Neg e) = case evalLCExp st e of
                            Right (Int n) -> Right (Int (-n))
                            _ -> Left TypeError
evalLCExp st (Not e) = case evalLCExp st e of
                            Right True -> Right False
                            Right False -> Right True
                            _ -> Left TypeError
evalLCExp st (Fst e) = case evalLCExp st e of
                            Right (Pair e1 _) -> evalLCExp e1
                            _ -> Left TypeError
evalLCExp st (Snd e) = case evalLCExp st e of
                            Right (Pair _ e2) -> evalLCExp e2
                            _ -> Left TypeError
evalLCExp st (Plus e1 e2) = case evalLCExp st e1 of
                                Right (Num x) -> case evalLCExp st e2 of
                                                    Right (Num y) -> Right (Num (x+y))
                                                    _ -> Left TypeError
                                _ -> Left TypeError
evalLCExp st (Minus e1 e2) = case evalLCExp st e1 of
                                Right (Num x) -> case evalLCExp st e2 of
                                                    Right (Num y) -> Right (Num (x-y))
                                                    _ -> Left TypeError
                                _ -> Left TypeError
evalLCExp st (Mult e1 e2) = case evalLCExp st e1 of
                                Right (Num x) -> case evalLCExp st e2 of
                                                    Right (Num y) -> Right (Num (x*y))
                                                    _ -> Left TypeError
                                _ -> Left TypeError
evalLCExp st (Div e1 e2) = case evalLCExp st e1 of
                                Right (Num x) -> case evalLCExp st e2 of
                                                    Right (Num y) -> Right (Num (x `div` y))
                                                    _ -> Left TypeError
                                _ -> Left TypeError



subst :: Store -> LCExp -> VarName -> LCExp -> LCExp
subst st (Var x) y e2 = if x == y then e2 else (Var x)
subst st (App e11 e12) x e2 = (App (subst st e11 x e2) (subst st e12 x e2))
subst st (Lambda x e1) y e2 = if x == y then (Lambda x e1) else (Lambda x (subst st e1 y e2))
