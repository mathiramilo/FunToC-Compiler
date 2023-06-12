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
    go (App name exprs) declaredNames errors = concatMap (\expr -> go expr declaredNames errors) exprs ++ if name `elem` declaredNames then errors else errors ++ [Undefined name]

-- ######################################
-- #############CHECKER 2.2##############
-- ######################################

-- Counts number of parameters from each function **declaration**
getFunctionParamCounts :: Defs -> [(String, Number)] -> [(String, Number)]
getFunctionParamCounts [] results = results
getFunctionParamCounts (((name, _) params _):xs) result = 
  getFunctionParamCounts xs (result:(name, length params))

-- Counts number of parameters from each function **expression**
getExpressionParamCounts :: Expr -> [(String, Number)]
getExpressionParamCounts (App name expressions) = 
  (name, length expressions) : (concatMap getExpressionParamCounts expressions)
getExpressionParamCounts (Infix operator expr_left expr_right) = 
  (getExpressionParamCounts expr_left) ++ (getExpressionParamCounts expressions)
getExpressionParamCounts (If condition_expr then_expr else_expr) = 
  (getExpressionParamCounts condition_expr) ++ (getExpressionParamCounts else_expr)
getExpressionParamCounts (Let expr1 expr2) =
  (getExpressionParamCounts expr1) ++ (getExpressionParamCounts expr2)
getExpressionParamCounts (_) = []

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
-- TODO: Arreglar getApp
checkParamNumbers (Program defs expressions) = 
  checkDictInconsistencies (getFunctionParamCounts [] defs) (concatMap getExpressionParamCounts expressions)

-- ########################################
-- ############# CHECKER 2.4 ##############
-- ########################################

-- HELPERS-----------
getFunctionDefinitionByName :: String -> [Def] -> Maybe FunDef
getFunctionDefinitionByName name defs = find (\((f_name, _) _ _) -> f_name == name) defs

getFunctionParamsType :: FunDef -> [Type]
getFunctionParamsType (FunDef (TypedFun (_, (param_types, _)) _ _)) = param_types

getFunctionType :: FunDef -> Type
getFunctionTypes (FunDef (TypedFun (_, (_, function_type)) _ _)) = function_type
------------------

getType :: (Expr | Type) -> Env -> Defs -> (String | Error)
getType (Var name) env _ = getType $ snd $ find (\name_type (fst name_type) == name) env
getType (IntLit _) _ _ = 'Int'
getType (TyInt _) _ _ = 'Int'
getType (BoolLit _) _ _ = 'Bool'
getType (TyBool _) _ _ = 'Bool'

getType (Infix (Eq|NEq|GTh|LTh|GEq|LEq) _ expr1 expr2) _ _ = --TODO: Less than para bools??
  'Bool' if ((getType then_expr) == (getType else_expr)) else False --TODO: función se llama dos veces
--'Int' if (all (\x -> x `elem` ['Int', 'Bool']) $ map getType [expr1, expr2]) else False
  
getType (Infix (Add|Sub|Mult|Div) _ expr1 expr2) _ _ =
  'Int' if (all (=='Int') $ map getType [expr1, expr2]) else False

getType (If (condition then_expr else_expr)) _ _ = 
  (getType then_expr) --TODO: función se llama dos veces y retornar error en vez de false
  if (getType then_expr) == 'Bool' && (getType then_expr) == (getType else_expr)
  else False

getType (Let ((to_substitute_name, to_substitute_type) substituted_expr final_expression)) env defs =
  (getType final_expression env defs)
  if (getType substituted_expr env defs) == (getType to_substitute_type env defs) 
    && (getType to_substitute_name env defs) == (getType to_substitute_type env defs)
    -- TODO: Quitar ultima condicion? Agregar checkeo extra de si x tiene el mismo tipo que las x en e'.
  else False

getType (App (Name name) expressions) env defs = --TODO: Releer letra asquerosa
  getFunctionType $ getFunctionDefinitionByName name
  if all (==True) $ zipWith (==) (map (\x -> getType x env defs) (getFunctionDefinitionByName name defs)) (map getType expressions)
  else False

-- TODO: agregar los checked
-- TODO: Hacer lets.
checkExpressionTypes :: Program -> Env -> Checked
checkExpressionTypes (Program defs expressions) env = 
  concatMap (\x -> getType x env defs) expressions --TODO: Agregar errores

  

-- ########################################
-- ################# ALL ##################
-- ########################################

checkProgram :: Program -> Checked
checkProgram = do