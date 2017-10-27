module HuttonRazor where

    data Expr = Lit Integer | Add Expr Expr

    -- 1
    eval :: Expr -> Integer
    eval (Lit i)     = i
    eval (Add e1 e2) = eval e1 + eval e2

    -- 2
    printExpr :: Expr -> String
    printExpr (Lit i) = show i
    printExpr (Add e1 e2) = 
        printExpr e1 ++ " + " ++ printExpr e2
