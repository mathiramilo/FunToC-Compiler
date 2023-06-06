----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de chequeo
--
-- Se debe implementar la funcion checkProgram que, dado un AST
-- que representa un programa, retorna Ok en caso de no encontrar errores, 
-- o la lista de errores encontrados en otro caso.   
----------------------------------------------------------------------------


module Checker where

import Syntax
-- se pueden agregar mas importaciones 
-- en caso de ser necesario

import Data.List
import Data.Maybe

-- CHECKER

data Checked = Ok | Wrong [Error]

data Error = Duplicated      Name
           | Undefined       Name
           | ArgNumDef       Name Int Int
           | ArgNumApp       Name Int Int
           | Expected        Type Type
            
instance Show Error where
 show (Duplicated      n)  = "Duplicated declaration: " ++ n
 show (Undefined       n)  = "Undefined: " ++ n
 show (ArgNumDef   n s d)
   = "The number of parameters in the definition of "++ n ++
     " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (ArgNumApp   n s d)
   = "The number of arguments in the application of: " ++ n ++
     " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (Expected    ty ty')
   = "Expected: " ++ show ty ++ " Actual: " ++ show ty'

-------------------------------------------------
-- data Type = TyInt | TyBool
--  deriving Eq

-- data Program = Program Defs Expr

-- type Name  = String

-- type Defs = [FunDef]

-- type TypedVar = (Name, Type)
-- type TypedFun = (Name, Sig)

-- data FunDef  = FunDef TypedFun [Name]  Expr

-- data Sig = Sig [Type] Type

-- data Expr = Var     Name
--           | IntLit  Integer
--           | BoolLit Bool
--           | Infix   Op Expr Expr
--           | If      Expr Expr Expr
--           | Let     TypedVar Expr Expr
--           | App     Name [Expr]

-- data Op = Add | Sub | Mult | Div
--         | Eq | NEq | GTh | LTh | GEq | LEq
--         deriving Eq

-- type Env = [TypedVar]

checkVariableNames :: Program -> [Error]
checkFunctionNames :: Program -> [Error]

funcion(Var a) = table
funcion(_ a) = tal

-- sum1 = sumacc 0
--   where sumacc s []      = s 
--         sumacc s (x:xs)  = sumacc (s + x) xs

-- fact n = factacc 1 n
-- where
-- factacc a 0 = a
-- factacc a n = factacc (a ∗ n) (n − 1)


checkVariableNames Program (Defs defs) (Expr [FunDef]) = -- foldl (\) []
  | (condicion) = resultado
  | (condicion) = resultado
checkVariableNames Var name defs listaErrores =
  | name in defs = 

checkVariableNames Var 

  para cada fundef
    if variable (hoja):
      if hoja not in fundef
        append variable errores 
    else
      checkVariableNames defs expresiones

# data Program = Program Defs Expr 

checkProgram :: Program -> Checked
checkProgram = do

