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



-- Primer chequeo, repeticion de nombres. Si no hay errores, se devuelve 'Ok'. Si hay errores, se devuelven los errores.
checkRepeatedNames :: Program -> Checked
checkRepeatedNames (Program defs _) = if (checkFunctionDeclarations (Program defs _) == []) && (checkParamsNames (Program defs _) == []) then Ok else Wrong (checkFunctionDeclarations (Program defs _) ++ checkParamsNames (Program defs _))

-- Chequeo de multiples declaraciones de una misma funcion. Dado un programa, esta funcion devuelve una lista de errores tal que cada vez que se repite la declaracion de una funcion se agrega un error 'Duplicated' a la lista.
checkFunctionDeclarations :: Program -> [Error]
checkFunctionDeclarations (Program defs _) = findRepeated (map getFunctionName defs)

-- Dada una definicion de funcion, devuelve el nombre de la funcion.
getFunctionName :: FunDef -> String
getFunctionName (FunDef (name, _) _ _) = name

-- Dada una lista de strings, devuelve una lista de errores de forma que cada vez que un string se repite se agrega el correspondiente error a la lista.
-- La funcion 'go' recibe una lista de strings, una lista de strings vacia y una lista de errores vacia. La lista de strings vacia se utiliza para ir guardando los strings que ya se recorrieron. La lista de errores vacia se utiliza para ir guardando los errores que se van encontrando y finalmente devolverlos.
findRepeated :: [String] -> [Error]
findRepeated xs = go xs [] []
  where go :: [String] -> [String] -> [Error] -> [Error]
        go [] _ errors = errors
        go (x:xs) ys errors
          | x `elem` ys = go xs ys (errors ++ [Duplicated x])
          | otherwise = go xs (ys ++ [x]) errors

-- Chequeo de nombres repetidos de parametros dentro de la declaracion de funciones. Dado un programa, esta funcion devuelve una lista de errores tal que cada vez que se repite un parametro en la declaracion de una funcion se agrega un error 'Duplicated' a la lista.
checkParamsNames :: Program -> [Error]
checkParamsNames (Program defs _) = findRepeatedParams (map getFunctionParams defs)

-- Dada una definicion de funcion, devuelve los parametros de la funcion.
getFunctionParams :: FunDef -> [String]
getFunctionParams (FunDef (_, _) params _) = params

-- Dada una lista de listas de strings, devuelve una lista de errores de forma que cada vez que un string se repite dentro de una sublista se agrega el correspondiente error a la lista.
findRepeatedParams :: [[String]] -> [Error]
findRepeatedParams xs = concatMap findRepeated xs


-- Tercer chequeo, nombres no declarados. Si no hay errores, se devuelve 'Ok'. Si hay errores, se devuelven los errores.
checkUndefinedNames :: Program -> Checked
checkUndefinedNames (Program defs expr) = if (checkUndefinedNamesFuncDecl (Program defs expr) == []) && (checkUndefinedNamesMain (Program defs expr) == []) then Ok else Wrong (checkUndefinedNamesFuncDecl (Program defs expr) ++ checkUndefinedNamesMain (Program defs expr))

-- Chequeo de nombres no declarados en la declaracion de funciones.
checkUndefinedNamesFuncDecl :: Program -> [Error]
checkUndefinedNamesFuncDecl (Program defs _) = concatMap (checkUndefinedNamesFuncDecl' defs)

checkUndefinedNamesFuncDecl' :: FunDef -> [Error]
checkUndefinedNamesFuncDecl' (FunDef (name, _) params expr) = checkUndefindedNamesExpr expr params

-- Chequeo de nombres no declarados en la expresion principal.
checkUndefinedNamesMain :: Program -> [Error]
checkUndefinedNamesMain (Program defs expr) = checkUndefindedNamesExpr expr (map getFunctionName defs)

-- Dada una expresion y una lista de nombres declarados, esta funcion devuelve una lista de errores tal que cada vez que se encuentra un nombre no declarado se agrega un error 'Undefined' a la lista.
checkUndefinedNamesExpr :: Expr -> [String] -> [Error]
checkUndefinedNamesExpr expr declaredNames = go expr declaredNames []
  where 
    go :: Expr -> [String] -> [Error] -> [Error]
    go (Var name) declaredNames errors
      | name `elem` declaredNames = errors
      | otherwise = errors ++ [Undefined name]
    go (IntLit _) _ errors = errors
    go (BoolLit _) _ errors = errors
    go (Infix _ expr1 expr2) declaredNames errors = go expr1 declaredNames (go expr2 declaredNames errors)
    go (If expr1 expr2 expr3) declaredNames errors = go expr1 declaredNames (go expr2 declaredNames (go expr3 declaredNames errors))
    go (Let (name, _) expr1 expr2) declaredNames errors = go expr1 declaredNames (go expr2 (declaredNames ++ [name]) errors)
    go (App _ exprs) declaredNames errors = concatMap (\expr -> go expr declaredNames errors) exprs


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



checkProgram :: Program -> Checked
checkProgram = do

