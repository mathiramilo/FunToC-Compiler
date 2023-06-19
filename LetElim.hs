----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de eliminación de LETs
--
-- Un LET (let x = e1 in e2) es eliminado si e1 es 
-- un literal entero o booleano. En ese caso se 
-- sustituyen las ocurrencias de x en e2 por e1, 
-- o sea, e2[e1/x]. 
----------------------------------------------------------------------------

module LetElim where

import Syntax
import Data.List

-- ELIMINACION DE LETs

isLiteral :: Expr -> Bool
isLiteral (BoolLit _) = True
isLiteral (IntLit _) = True
isLiteral _ = False

letElimP :: Program -> Program 
letElimP (Program defs x) = Program (new_defs) (new_main)
    where 
        go :: Expr -> Expr
        go (Var expr) = Var expr
        go (IntLit expr) = IntLit expr
        go (BoolLit expr) = BoolLit expr
        go (Infix op expr1 expr2) = Infix op (go expr1) (go expr2)
        go (If condition then_expr else_expr) = If (go condition) (go then_expr) (else_expr)
        go original_expr@(Let typedvar@(name, _) expr1 expr2) = rechecked_final_expression
            where
                new_expr1 = go expr1
                new_expr2 = go expr2
                is_literal_expr = isLiteral new_expr1
                final_new_expression = 
                    if is_literal_expr
                    then subst name new_expr1 new_expr2 
                    else Let typedvar new_expr1 new_expr2
                rechecked_final_expression = 
                    if (show original_expr) /= (show final_new_expression)
                    then (go final_new_expression)
                    else final_new_expression
        go (App name expr) = App name (map go expr)

        new_defs = map (\(FunDef typedfun names expr) -> FunDef typedfun names (go expr)) defs
        new_main = go x

subst :: Name -> Expr -> Expr -> Expr 
subst name expr1 (Var expr2) = if name == expr2 then expr1 else Var expr2
subst name expr1 (IntLit expr2) = IntLit expr2
subst name expr1 (BoolLit expr2) = BoolLit expr2

subst name expr1 (If condition then_expr else_expr) = 
    If (subst name expr1 condition) (subst name expr1 then_expr) (subst name expr1 else_expr)
subst name expr1 (Infix operator operand1 operand2) =
    Infix operator (subst name expr1 operand1) (subst name expr1 operand2)
subst name expr1 (Let typedvar@(name_let, _) expr1_let expr2_let)
    -- If too lets with same variable name, only substitute the first expression
    | name == name_let = Let typedvar (subst name expr1 expr1_let) expr2_let
    -- Otherwise, substitute both
    | otherwise = Let typedvar (subst name expr1 expr1_let) (subst name expr1 expr2_let) -- (subst name_let expr1_let expr2_let)
subst name expr1 (App app_name expressions) = App app_name $ map (subst name expr1) expressions