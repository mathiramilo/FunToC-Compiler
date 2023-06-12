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

letElimP :: Program -> Program 
letElimP Program (Defs defs) (Expr x) = Program defs $ go x
                                        where go ((Var|Integer|BoolLit) expr) = expr
                                              go (Infix op expr1 expr2) = Infix (go op) (go expr1) (go expr2)
                                              go (If condition then_expr else_expr) = (go condition) (go then_expr) (else_expr) 
                                              go (Let (name, _) expr1 expr2) = subst name expr1 expr2
                                              go (App name expr) = App name $ map go expr

subst :: Name -> Expr -> Expr -> Expr 
subst name expr1 (Name expr2) = if name == expr2 then expr2 else expr1
subst name expr1 ((Integer|BoolLit) expr2) = expr2
subst name expr1 (If condition then_expr else_expr) = 
    If (subst name condition) (subst name then_expr) (subst name else_expr)
subst name expr1 (Infix Op operand1 operand2) = 
    Infix Op (subst name operand1) (subst name operand2)
subst name expr1 (Let (TypedVar (name_let, type)) expr1_let expr2_let) =
    subst name expr1 (subst name_let expr1_let expr2_let)
subst name expr1 (App app_name expressions) = App app_name $ map (subst name) expressions