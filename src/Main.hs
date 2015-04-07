module Main where

import Data.List  

type Name = String
data Expr = Var Name | App Expr Expr | Lam Name Expr deriving (Show)
--Interp. This data is defined for simple user input

type DBName = Int
data DBExpr = DBVar DBName | DBApp DBExpr DBExpr | DBLam DBExpr deriving (Show)
--Interp. This data represent data for simple alghoritm of interpreter lambda-calculus
--De Bruijn indices


----------------------------------------------
--Interp. get count of Lam from Expr
num_for_free :: Expr -> Int
num_for_free (App ex1 ex2) = num_for_free (ex1) + num_for_free (ex2)
num_for_free (Lam name ex) = 1 + num_for_free ex
num_for_free (Var name) = 0

--Interp. get count of DBLam from DBExpr
num_for_freeDB :: DBExpr -> Int
num_for_freeDB (DBApp ex1 ex2) = num_for_freeDB (ex1) + num_for_freeDB (ex2)
num_for_freeDB (DBLam ex) = 1 + num_for_freeDB ex
num_for_freeDB (DBVar name) = 0


----------------------------------------------
--Interp. convert Expr to DBExpr
expr_to_dbexpr :: Expr -> DBExpr
expr_to_dbexpr expr = let
                         
                         p = num_for_free expr 
                          
                         go (App ex1 ex2) index list = DBApp (go ex1 index list) (go ex2 index list) 
                         go (Lam name (Var ex)) index list = if (elemIndices ex list) /= []
                                                                 then DBLam (go (Var ex) (index) (name:list))
                                                             else DBLam (go (Var ex) (index+p) list)
                         go (Lam name ex) index list = DBLam (go ex (index-1) (name:list))
                         go (Var name) index list = if (elemIndices name list) /= []
                                                        then DBVar ((elemIndices name list)!!0) 
                                                    else DBVar index
    
                      in go expr (num_for_free expr) []

---------------------------------------------
--Interp. Shift (adding for B-reduction)
shift :: Int -> Int -> DBExpr -> DBExpr
shift d c (DBVar k) = if (k <= c)
                        then DBVar k
                    else DBVar (k+d)
shift d c (DBLam t1) = DBLam (shift d (c+1) t1)
shift d c (DBApp t1 t2) = DBApp (shift d c t1) (shift d c t2)


---------------------------------------------
--Interp. Substitution (adding for B-reduction)
-- [j -> s] t
subs :: DBName -> DBExpr -> DBExpr -> DBExpr
subs j s (DBVar k) = if (k == j)
                         then s
                     else (DBVar k)
subs j s (DBLam t1) = DBLam (subs (j+1) (shift 1 0 s) t1)
subs j s (DBApp t1 t2) = DBApp (subs j s t1) (subs j s t2)


---------------------------------------------
--Interp. B-reduction (1 step)
eval :: DBExpr -> DBExpr -> DBExpr
eval t12 v2 = shift (-1) 0 (subs 0 (shift 1 0 v2) t12)


---------------------------------------------
--Interp. evaln
evaln :: DBExpr -> DBExpr
evaln (DBApp t1 t2) = eval (evaln (t1)) (evaln (t2)) 
evaln (DBLam (DBVar t)) = (DBLam (DBVar t)) 
evaln (DBLam t) = (DBLam (evaln (t)))
evaln (DBVar t) = DBVar t


---------------------------------------------
--Interp. convert DBExpr to Expr
dbexpr_to_expr :: DBExpr -> [Name] -> Expr
dbexpr_to_expr t list = let
                            go (DBApp t1 t2) list num = App (go t1 list num) (go t2 list num)
                            go (DBLam t1) list num = Lam (list!!num) (go t1 list (num-1))
                            go (DBVar t1) list num = Var (list!!t1)
                        in go t list ((num_for_freeDB t)-1)


main = print (dbexpr_to_expr (DBLam (DBLam (DBVar 0))) ["x","y","z","d","e"])

-- test-input
-----------------------------------------
--App (Lam "s" (Lam "w" (Var "s"))) (Var "a"))


--__input1
--expr_to_dbexpr (App (Lam "q" (Lam "s" (Lam "k" (Var "q")))) (Lam "q" (Lam "p" (Lam "k" (Var "p")))))
--__output1
--DBApp (DBLam (DBLam (DBLam (DBVar 2)))) (DBLam (DBLam (DBLam (DBVar 1))))

--__input2
--shift 2 0 (DBApp (DBLam (DBApp (DBLam (DBVar 1)) (DBVar 2))) (DBVar 0))
--__output2
--DBApp (DBLam (DBApp (DBLam (DBVar 1)) (DBVar 4))) (DBVar 0)

--__input3
--subs 0 (DBVar 1) (DBApp (DBLam (DBVar 0)) (DBVar 2))
--__output3
--DBApp (DBLam (DBVar 0)) (DBVar 2)

--__input4
--evaln (DBApp (DBLam (DBLam (DBLam (DBVar 2)))) (DBVar 2))
--__output4
--DBLam (DBLam (DBVar 4))

--__input5
--dbexpr_to_expr (DBLam (DBLam (DBVar 0))) ["x","y","z","d","e"]
--__output5
--Lam "y" (Lam "x" (Var "x"))


