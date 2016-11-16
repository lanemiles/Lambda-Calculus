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
eval st ((LetS x e):xs) = case evalLCExp st e of
        (Left err) -> (Left err)
        (Right e') -> eval (Map.insert x e' st) xs
eval st ((LetRecS x _ e):xs) = case e of
                                Lambda _ _ _ -> eval (Map.insert x e st) xs
                                _ -> Left RecError
eval st ((LCExp e):xs) = (:) <$> evalLCExp st e <*> eval st xs

evalLCExp :: Store -> LCExp -> Either Error LCExp
evalLCExp st (Var k) = case Map.lookup k st of
                            Nothing -> Left (UnboundVariable k)
                            (Just v) -> Right v
evalLCExp st (App e1 e2) =  case evalLCExp st e1 of
                                Left err -> Left err
                                Right (Lambda x _ e1') -> case evalLCExp st e2 of
                                    Left err -> Left err
                                    Right e2' -> evalLCExp st (subst st e1' x e2')
evalLCExp st (Lambda x t e) = Right (Lambda x t e)
evalLCExp st (Cond e1 e2 e3) = case evalLCExp st e1 of
                                    Left err -> Left err
                                    Right AST.True -> evalLCExp st e2
                                    Right AST.False -> evalLCExp st e3
                                    _ -> Left TypeError
evalLCExp st (HasType e _) = evalLCExp st e
evalLCExp _ (Num n) = Right (Num n)
evalLCExp _ AST.True = Right AST.True
evalLCExp _ AST.False = Right AST.False
evalLCExp st (Pair e1 e2) = Right Pair <*> (evalLCExp st e1) <*> (evalLCExp st e2)
evalLCExp st (Neg e) = case evalLCExp st e of
                            Left err -> Left err
                            Right (Num n) -> Right (Num (-n))
                            _ -> Left TypeError
evalLCExp st (Not e) = case evalLCExp st e of
                            Left err -> Left err
                            Right AST.True -> Right AST.False
                            Right AST.False -> Right AST.True
                            _ -> Left TypeError
evalLCExp st (Fst e) = case evalLCExp st e of
                            Left err -> Left err
                            Right (Pair e1 _) -> Right e1
                            _ -> Left TypeError
evalLCExp st (Snd e) = case evalLCExp st e of
                            Left err -> Left err
                            Right (Pair _ e2) -> Right e2
                            _ -> Left TypeError
evalLCExp st (Plus e1 e2) = case evalLCExp st e1 of
                                Left err -> Left err
                                Right (Num x) -> case evalLCExp st e2 of
                                                    Left err -> Left err
                                                    Right (Num y) -> Right (Num (x + y))
                                                    _ -> Left TypeError
                                _ -> Left TypeError
evalLCExp st (Minus e1 e2) = case evalLCExp st e1 of
                                Left err -> Left err
                                Right (Num x) -> case evalLCExp st e2 of
                                                    Left err -> Left err
                                                    Right (Num y) -> Right (Num (x - y))
                                                    _ -> Left TypeError
                                _ -> Left TypeError
evalLCExp st (Mult e1 e2) = case evalLCExp st e1 of
                                Left err -> Left err
                                Right (Num x) -> case evalLCExp st e2 of
                                                    Left err -> Left err
                                                    Right (Num y) -> Right (Num (x * y))
                                                    _ -> Left TypeError
                                _ -> Left TypeError
evalLCExp st (Div e1 e2) = case evalLCExp st e1 of
                                Left err -> Left err
                                Right (Num x) -> case evalLCExp st e2 of
                                                    Left err -> Left err
                                                    Right (Num y) -> Right (Num (x `div` y))
                                                    _ -> Left TypeError
                                _ -> Left TypeError
evalLCExp st (And e1 e2) = case evalLCExp st e1 of
                                Left err -> Left err
                                Right AST.True -> case evalLCExp st e2 of
                                                Left err -> Left err
                                                Right AST.True -> Right AST.True
                                                Right AST.False -> Right AST.False
                                                _ -> Left TypeError
                                Right AST.False -> case evalLCExp st e2 of
                                                Left err -> Left err
                                                Right AST.True -> Right AST.False
                                                Right AST.False -> Right AST.False
                                                _ -> Left TypeError
                                _ -> Left TypeError
evalLCExp st (Or e1 e2) = case evalLCExp st e1 of
                                Left err -> Left err
                                Right AST.True -> case evalLCExp st e2 of
                                                Left err -> Left err
                                                Right AST.True -> Right AST.True
                                                Right AST.False -> Right AST.True
                                                _ -> Left TypeError
                                Right AST.False -> case evalLCExp st e2 of
                                                Left err -> Left err
                                                Right AST.True -> Right AST.True
                                                Right AST.False -> Right AST.False
                                                _ -> Left TypeError
                                _ -> Left TypeError
evalLCExp st (Eq e1 e2) = case evalLCExp st e1 of
                                Left err -> Left err
                                Right AST.True -> case evalLCExp st e2 of
                                                Left err -> Left err
                                                Right AST.True -> Right AST.True
                                                Right AST.False -> Right AST.False
                                                _ -> Left TypeError
                                Right AST.False -> case evalLCExp st e2 of
                                                Left err -> Left err
                                                Right AST.True -> Right AST.False
                                                Right AST.False -> Right AST.True
                                                _ -> Left TypeError
                                Right (Num x) -> case evalLCExp st e2 of
                                                Left err -> Left err
                                                Right (Num y) -> if x == y then Right AST.True else Right AST.False
                                                _ -> Left TypeError
                                Right (Pair e11 e12) -> case evalLCExp st e2 of
                                                Left err -> Left err
                                                Right (Pair e21 e22) -> evalLCExp st (And (Eq e11 e21) (Eq e12 e22))
                                                _ -> Left TypeError
                                _ -> Left TypeError
evalLCExp st (Let x e1 e2) = evalLCExp st (App (Lambda x undefined e2) e1)
evalLCExp st (LetRec x _ e1 e2) = case e1 of
                                    Lambda _ _ _ -> evalLCExp (Map.insert x e1 st) e2
                                    _ -> Left RecError


subst :: Store -> LCExp -> VarName -> LCExp -> LCExp
subst st (Var x) y e2 = if x == y then e2 else (Var x)
subst st (App e11 e12) x e2 = (App (subst st e11 x e2) (subst st e12 x e2))
subst st (Lambda x t e1) y e2 = if x == y then (Lambda x t e1) else (Lambda x t (subst st e1 y e2))
subst st (Cond e1 e2 e3) x e' = Cond (subst st e1 x e') (subst st e2 x e') (subst st e3 x e')
subst st (HasType e t) x e2 = HasType (subst st e x e2) t
subst st (Pair e1 e2) x e = Pair (subst st e1 x e) (subst st e2 x e)
subst st (Neg e) x e2 = Neg (subst st e x e2)
subst st (Not e) x e2 = Not (subst st e x e2)
subst st (Fst e) x e2 = Fst (subst st e x e2)
subst st (Snd e) x e2 = Snd (subst st e x e2)
subst st (Plus e1 e2) x e = Plus (subst st e1 x e) (subst st e2 x e)
subst st (Minus e1 e2) x e = Minus (subst st e1 x e) (subst st e2 x e)
subst st (Mult e1 e2) x e = Mult (subst st e1 x e) (subst st e2 x e)
subst st (Div e1 e2) x e = Div (subst st e1 x e) (subst st e2 x e)
subst st (And e1 e2) x e = And (subst st e1 x e) (subst st e2 x e)
subst st (Or e1 e2) x e = Or (subst st e1 x e) (subst st e2 x e)
subst st (Eq e1 e2) x e = Eq (subst st e1 x e) (subst st e2 x e)
subst st (Let y e1 e2) x e = Let y (subst st e1 x e) (subst st e2 x e)
subst st (LetRec y t e1 e2) x e = LetRec y t (subst st e1 x e) (subst st e2 x e)
subst _ e _ _ = e


evalTypesAst :: [Statement] -> Either Error [Type]
evalTypesAst p = if null p then Left Empty else evalTypes Map.empty p

evalTypes :: Context -> [Statement] -> Either Error [Type]
evalTypes ctx [] = Right []
evalTypes ctx ((LetS x e):xs) = case typeOf ctx e of
                                    Left err -> Left err
                                    Right t -> evalTypes (Map.insert x t ctx) xs
evalTypes ctx ((LetRecS x t e):xs) = case typeOf (Map.insert x t ctx) e of
                                        Left err -> Left err
                                        Right t' -> if t == t' then evalTypes (Map.insert x t ctx) xs else Left TypeError
evalTypes ctx ((LCExp e):xs) = (:) <$> typeOf ctx e <*> evalTypes ctx xs

typeOf :: Context -> LCExp -> Either Error Type
typeOf ctx (Var k) = case Map.lookup k ctx of
                            Nothing -> Left (UnboundVariable k)
                            Just v -> Right v
typeOf ctx (App e1 e2) =  case typeOf ctx e1 of
                                Left err -> Left err
                                Right (Func t1 t2) -> case typeOf ctx e2 of
                                                        Left err -> Left err
                                                        Right t -> if t == t1 then Right t2 else Left TypeError
                                _ -> Left TypeError
typeOf ctx (Lambda x t1 e) = case typeOf (Map.insert x t1 ctx) e of
                                Left err -> Left err
                                Right t2 -> Right (Func t1 t2)
typeOf ctx (Cond e1 e2 e3) = case typeOf ctx e1 of
                                    Left err -> Left err
                                    Right Bool -> case typeOf ctx e2 of
                                                    Left err -> Left err
                                                    Right t1 -> case typeOf ctx e3 of
                                                                    Left err -> Left err
                                                                    Right t2 -> if t1 == t2 then Right t1 else Left TypeError
                                    _ -> Left TypeError
typeOf ctx (HasType e t) = case typeOf ctx e of
                            Left err -> Left err
                            Right t' -> if t == t' then Right t else Left TypeError
typeOf _ (Num n) = Right Int
typeOf _ AST.True = Right Bool
typeOf _ AST.False = Right Bool
typeOf ctx (Pair e1 e2) = Right Tuple <*> (typeOf ctx e1) <*> (typeOf ctx e2)
typeOf ctx (Neg e) = case typeOf ctx e of
                            Left err -> Left err
                            Right Int -> Right Int
                            _ -> Left TypeError
typeOf ctx (Not e) = case typeOf ctx e of
                            Left err -> Left err
                            Right Bool -> Right Bool
                            _ -> Left TypeError
typeOf ctx (Fst e) = case typeOf ctx e of
                            Left err -> Left err
                            Right (Tuple e1 _) -> Right e1
                            _ -> Left TypeError
typeOf ctx (Snd e) = case typeOf ctx e of
                            Left err -> Left err
                            Right (Tuple _ e2) -> Right e2
                            _ -> Left TypeError
typeOf ctx (Plus e1 e2) = case typeOf ctx e1 of
                                Left err -> Left err
                                Right Int -> case typeOf ctx e2 of
                                                Left err -> Left err
                                                Right Int -> Right Int
                                                _ -> Left TypeError
                                _ -> Left TypeError
typeOf ctx (Minus e1 e2) = case typeOf ctx e1 of
                                Left err -> Left err
                                Right Int -> case typeOf ctx e2 of
                                                Left err -> Left err
                                                Right Int -> Right Int
                                                _ -> Left TypeError
                                _ -> Left TypeError
typeOf ctx (Mult e1 e2) = case typeOf ctx e1 of
                                Left err -> Left err
                                Right Int -> case typeOf ctx e2 of
                                                Left err -> Left err
                                                Right Int -> Right Int
                                                _ -> Left TypeError
                                _ -> Left TypeError
typeOf ctx (Div e1 e2) = case typeOf ctx e1 of
                                Left err -> Left err
                                Right Int -> case typeOf ctx e2 of
                                                Left err -> Left err
                                                Right Int -> Right Int
                                                _ -> Left TypeError
                                _ -> Left TypeError
typeOf ctx (And e1 e2) = case typeOf ctx e1 of
                                Left err -> Left err
                                Right Bool -> case typeOf ctx e2 of
                                                Left err -> Left err
                                                Right Bool -> Right Bool
                                                _ -> Left TypeError
                                _ -> Left TypeError
typeOf ctx (Or e1 e2) = case typeOf ctx e1 of
                                Left err -> Left err
                                Right Bool -> case typeOf ctx e2 of
                                                Left err -> Left err
                                                Right Bool -> Right Bool
                                                _ -> Left TypeError
                                _ -> Left TypeError
typeOf ctx (Eq e1 e2) = case typeOf ctx e1 of
                                Left err -> Left err
                                Right Bool -> case typeOf ctx e2 of
                                                Left err -> Left err
                                                Right Bool -> Right Bool
                                                _ -> Left TypeError
                                Right Int -> case typeOf ctx e2 of
                                                Left err -> Left err
                                                Right Int -> Right Bool
                                                _ -> Left TypeError
                                Right (Tuple e11 e12) -> case typeOf ctx e2 of
                                                Left err -> Left err
                                                Right (Tuple e21 e22) -> if e11 == e21 && e12 == e22 then Right Bool else Left TypeError
                                                _ -> Left TypeError
                                _ -> Left TypeError
typeOf ctx (Let x e1 e2) = case typeOf ctx e1 of
                                Left err -> Left err
                                Right t1 -> typeOf (Map.insert x t1 ctx) e2
typeOf ctx (LetRec x t1 e1 e2) = case typeOf (Map.insert x t1 ctx) e1 of
                                    Left err -> Left err
                                    Right t1' -> if t1 /= t1' then Left TypeError else typeOf (Map.insert x t1 ctx) e2 
