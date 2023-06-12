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
letElimP (Program defs x) = Program defs (go x)
                                    where go :: Expr -> Expr
                                          go (Var expr) = Var expr
                                          go (IntLit expr) = IntLit expr
                                          go (BoolLit expr) = BoolLit expr
                                          go (Infix op expr1 expr2) = Infix op (go expr1) (go expr2)
                                          go (If condition then_expr else_expr) = If (go condition) (go then_expr) (else_expr) 
                                          go (Let (name, _) expr1 expr2) = subst name expr1 expr2
                                          go (App name expr) = App name (map go expr)

subst :: Name -> Expr -> Expr -> Expr 
subst name expr1 (Var expr2) = if name == expr2 then Var expr2 else expr1
subst name expr1 (IntLit expr2) = IntLit expr2
subst name expr1 (BoolLit expr2) = BoolLit expr2

subst name expr1 (If condition then_expr else_expr) = 
    If (subst name expr1 condition) (subst name expr1 then_expr) (subst name expr1 else_expr)
subst name expr1 (Infix operator operand1 operand2) = --PELIGROSO OPT
    Infix operator (subst name expr1 operand1) (subst name expr1 operand2)
subst name expr1 (Let (name_let, _) expr1_let expr2_let) =
    subst name expr1 (subst name_let expr1_let expr2_let)
subst name expr1 (App app_name expressions) = App app_name $ map (subst name expr1) expressions