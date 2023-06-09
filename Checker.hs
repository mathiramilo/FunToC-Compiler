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

-- ######################################
-- #############CHECKER 2.1##############
-- ######################################

-- Chequeo de multiples declaraciones de una misma funcion. Dado un programa, esta funcion devuelve una lista de errores tal que cada vez que se repite la declaracion de una funcion se agrega un error 'Duplicated' a la lista.
checkFunctionDeclarations :: Program -> [Error]
checkFunctionDeclarations (Program defs _) = findRepeated (map getFunctionName defs)

-- Dada una definicion de funcion, devuelve el nombre de la funcion.
getFunctionName :: Def -> String
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
getFunctionParams :: Def -> [String]
getFunctionParams (FunDef (_, _) params _) = params

-- Dada una lista de listas de strings, devuelve una lista de errores de forma que cada vez que un string se repite dentro de una sublista se agrega el correspondiente error a la lista.
findRepeatedParams :: [[String]] -> [Error]
findRepeatedParams xs = concatMap findRepeated xs

-- Primer chequeo, repeticion de nombres. Si no hay errores, se devuelve 'Ok'. Si hay errores, se devuelven los errores.
checkRepeatedNames :: Program -> Checked
-- TODO: Se está llamando a la función checkFunctionDeclarations dos veces
checkRepeatedNames (Program defs _) = if (checkFunctionDeclarations (Program defs _) == []) && (checkParamsNames (Program defs _) == []) then Ok else Wrong (checkFunctionDeclarations (Program defs _) ++ checkParamsNames (Program defs _))

-- ######################################
-- #############CHECKER 2.2##############
-- ######################################


-- Hallar mapa de valores {(name: param_num)}
-- Hallar mapa de llamadas {(name: param_num)}
-- Checkear que se correspondan

-- data FunDef  = FunDef TypedFun [Name]  Expr
-- data Expr = Var     Name
--           | IntLit  Integer
--           | BoolLit Bool
--           | Infix   Op Expr Expr
--           | If      Expr Expr Expr
--           | Let     TypedVar Expr Expr
--           | App     Name [Expr]

-- Gets main branch of application
getApp :: [Expr] -> Just App | Nothing
getApp (((App app):_)) = app
getApp (((_):xs)) = getApp xs
getApp [] = Nothing

-- Counts number of parameters from each function **declaration**
getFunctionParamCounts :: Defs -> [(String, Number)]
getFunctionParamCounts [] results = results
getFunctionParamCounts (((name _) params _):xs) result = 
  getFunctionParamCounts xs (result:(name, length params))

-- Counts number of parameters from each function **expression**
getExpressionParamCounts :: Expr -> [(String, Number)]
getExpressionParamCounts (App _ expressions) = 
  concatMap getExpressionParamCounts expressions
getExpressionParamCounts (Infix)
getExpressionParamCounts (If)
getExpressionParamCounts (Let)  -- y las funciones????

-- For a given a key-pair tuple, 
-- asserts that for each key-value pair in a list of tuples, 
-- there is no key so that key1 == key2 and value1 != value2.
checkDictInconsistency :: (String, Number) -> [(String, Number)] -> Bool
checkDictInconsistency (key, value) [] = True
checkDictInconsistency (key, value) ((other_key, other_value):xs)
  | key==other_key && value == other_value = False
  | otherwise = checkDictInconsistency (key, value) (xs)

checkDictInconsistencies :: [(String, Number)] -> [(String, Number)] -> Bool
checkDictInconsistencies tuples1 tuples2 = all (==True) $ map (\tuple -> checkDictInconsistency tuple tuples2) tuples1

checkParamNumbers :: Program -> Checked
checkParamNumbers (Program defs expressions) = checkDictInconsistencies (getFunctionParamCounts defs) (getExpressionParamCounts $ getApp defs)

checkProgram :: Program -> Checked
checkProgram = do

