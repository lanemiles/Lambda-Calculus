module Interp where
import AST
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

evalAstNum :: [Statement] -> Either Error [Int]
evalAstNum = eval' Map.empty

eval' :: Store -> [Statement] -> Either Error [Int]
eval' st [] = Right []
eval' st ((Let x e):xs) = case evalLCExp st e of
        (Left err) -> (Left err)
        (Right e') -> eval' (Map.insert x e' st) xs
eval' st ((LCExp e):xs) = (:) <$> evalLCExp' st e <*> eval' st xs

evalLCExp' :: Store -> LCExp -> Either Error Int
evalLCExp' st e = case evalLCExp st (App (App e Succ) Zero) of
                        Left err -> Left err
                        Right e' -> toInt 0 e'

toInt :: Int -> LCExp -> Either Error Int
toInt n (SuccX e) = toInt (n+1) e
toInt n Zero = Right n
toInt _ _ = Left NotNum

evalAst :: [Statement] -> Either Error [LCExp]
evalAst = eval Map.empty

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
evalLCExp st (App e1 e2) =  case evalLCExp st e2 of
                                Left err -> Left err
                                (Right e2') -> case evalLCExp st e1 of
                                    Left err -> Left err
                                    Right (Lambda x e1') -> evalLCExp st (subst st e1' x e2')
                                    Right Succ -> Right (SuccX e2')
evalLCExp st (Lambda x e) = Right (Lambda x e)
evalLCExp _ Zero = Right Zero
evalLCExp _ Succ = Right Succ
evalLCExp _ (SuccX e) = Right (SuccX e)



subst :: Store -> LCExp -> VarName -> LCExp -> LCExp
subst st (Var x) y e2 = if x == y then e2 else (Var x)
subst st (App e11 e12) x e2 = (App (subst st e11 x e2) (subst st e12 x e2))
subst st (Lambda x e1) y e2 = if x == y then (Lambda x e1) else (Lambda x (subst st e1 y e2))
subst _ Succ _ _ = Succ
subst _ Zero _ _ = Zero



freeListify :: [Statement] -> [VarName]
freeListify stmts = Set.toAscList(undef)
	where (def, undef) = free (Set.empty, Set.empty) stmts

free :: (Set VarName, Set VarName) -> [Statement] -> (Set VarName, Set VarName)
free (defs, undefs) [] = (defs, undefs)
free (defs, undefs) ((Let x e):xs) = free (Set.insert x defs , Set.union (freeLCExp defs e) undefs) xs
free (defs, undefs) ((LCExp e):xs) = free (defs, Set.union (freeLCExp defs e) undefs) xs


freeLCExp :: Set VarName -> LCExp -> Set VarName
freeLCExp defs (Var x) = if x `elem` defs then Set.empty else Set.insert x Set.empty
freeLCExp defs (App e1 e2) = (freeLCExp defs e1) `Set.union` (freeLCExp defs e2)
freeLCExp defs (Lambda x e) = (freeLCExp defs e) `Set.difference` (Set.insert x Set.empty)