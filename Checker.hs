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
checkRepeatedNames :: Program -> [Error]
checkRepeatedNames prog = if (checkFunctionDeclarations prog == []) && ((checkParamsNames prog) == []) then [] else (checkFunctionDeclarations prog ++ checkParamsNames prog)

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

-- ######################################
-- #############CHECKER 2.3##############
-- ######################################

-- Tercer chequeo, nombres no declarados. Si no hay errores, se devuelve 'Ok'. Si hay errores, se devuelven los errores.
checkUndefinedNames :: Program -> [Error]
checkUndefinedNames prog = if (checkUndefinedNamesFuncDecl prog == []) && (checkUndefinedNamesMain prog == []) then [] else (checkUndefinedNamesFuncDecl prog ++ checkUndefinedNamesMain prog)

-- Chequeo de nombres no declarados en la declaracion de funciones.
checkUndefinedNamesFuncDecl :: Program -> [Error]
checkUndefinedNamesFuncDecl (Program defs _) = concatMap checkUndefinedNamesFuncDecl' defs

checkUndefinedNamesFuncDecl' :: FunDef -> [Error]
checkUndefinedNamesFuncDecl' (FunDef (name, _) params expr) = checkUndefinedNamesExpr expr params

-- Chequeo de nombres no declarados en la expresion principal.
checkUndefinedNamesMain :: Program -> [Error]
checkUndefinedNamesMain (Program defs expr) = checkUndefinedNamesExpr expr (map getFunctionName defs)

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
getFunctionParamCounts :: Defs -> [(String, Int)] -> [(String, Int)]
getFunctionParamCounts [] results = results
getFunctionParamCounts ((FunDef (name, _) params _):xs) result = 
  getFunctionParamCounts xs ((name, length params):result)

-- Counts number of parameters from each function **expression**
getExpressionParamCounts :: Expr -> [(String, Int)]
getExpressionParamCounts (App name expressions) = 
  (name, length expressions) : (concatMap getExpressionParamCounts expressions)
getExpressionParamCounts (Infix operator expr_left expr_right) = 
  (getExpressionParamCounts expr_left) ++ (getExpressionParamCounts expr_right)
getExpressionParamCounts (If condition_expr then_expr else_expr) = 
  (getExpressionParamCounts condition_expr) ++ (getExpressionParamCounts then_expr) ++ (getExpressionParamCounts else_expr)
getExpressionParamCounts (Let operator expr1 expr2) =
  (getExpressionParamCounts expr1) ++ (getExpressionParamCounts expr2)
getExpressionParamCounts (_) = []

-- For a given a key-pair tuple, 
-- asserts that for each key-value pair in a list of tuples, 
-- there is no key so that key1 == key2 and value1 != value2.
checkDictInconsistency :: (String, Int) -> [(String, Int)] -> Bool
checkDictInconsistency (key, value) [] = True
checkDictInconsistency (key, value) ((other_key, other_value):xs)
  | key==other_key && value == other_value = False
  | otherwise = checkDictInconsistency (key, value) xs

checkDictInconsistencies :: [(String, Int)] -> [(String, Int)] -> Bool
checkDictInconsistencies tuples1 tuples2 = all (==True) $ map (\tuple -> checkDictInconsistency tuple tuples2) tuples1

checkParamInts :: Program -> Checked
-- TODO: Arreglar getApp
checkParamInts (Program defs expr) = 
  if
  checkDictInconsistencies (getFunctionParamCounts defs []) (getExpressionParamCounts expr)
  then Ok else Wrong []

-- ########################################
-- ############# CHECKER 2.4 ##############
-- ########################################

-- HELPERS-----------
getFunctionDefinitionByName :: String -> Defs -> Maybe FunDef
getFunctionDefinitionByName name defs = find (\(FunDef (f_name, _) _ _) -> f_name == name) defs

getFunctionParamsType :: FunDef -> [Type]
getFunctionParamsType (FunDef (_, (Sig param_types  _)) _ _) = param_types

getFunctionType :: FunDef -> Type
getFunctionType (FunDef (_, (Sig _  function_type)) _ _) = function_type
------------------

handleExpressionComparison :: Expr -> Expr -> String -> String
handleExpressionComparison expr1 expr2 type_expression = --TODO: Less than para bools??
  if ((getType (expr1) [] []) == (getType (expr2) [] [])) then (getType (type_expression) [] []) else "error"

handleArithmeticComparison :: Expr -> Expr -> String
handleArithmeticComparison expr1 expr2 =
  if (all (=="Int") $ map getType [expr1, expr2]) then "Int" else "False"

getType :: Either Expr Type -> Env -> Defs -> String
getType (Left (Var name)) env _ = getType $ snd $ find (\name_type -> (fst name_type) == name) env
getType (Left (IntLit _)) _ _ = "Int"
getType (Right TyInt) _ _ = "Int"

getType (Left (BoolLit _)) _ _ = "Bool"
getType (Right TyBool) _ _ = "Bool"

getType (Infix Eq expr1 expr2) _ _ =
  handleExpressionComparison expr1 expr2 "Bool"
getType (Infix NEq expr1 expr2) _ _ =
  handleExpressionComparison expr1 expr2 "Bool"
getType (Infix GTh expr1 expr2) _ _ =
  handleExpressionComparison expr1 expr2 "Bool"
getType (Infix LTh expr1 expr2) _ _ =
  handleExpressionComparison expr1 expr2 "Bool"
getType (Infix GEq expr1 expr2) _ _ =
  handleExpressionComparison expr1 expr2 "Bool"
getType (Infix LEq expr1 expr2) _ _ =
  handleExpressionComparison expr1 expr2 "Bool"

-- getType (Infix (Either Eq (Either NEq (Either GTh (Either LTh (Either GEq LEq))))) _ expr1 expr2) _ _ = --TODO: Less than para bools??
--   if ((getType expr1) == (getType expr2)) then "Bool" else 'error' --TODO: función se llama dos veces
--"Int" if (all (\x -> x `elem` ["Int", "Bool"]) $ map getType [expr1, expr2]) else False

getType (Infix Add expr1 expr2) _ _ = handleArithmeticComparison expr1 expr2
getType (Infix Sub expr1 expr2) _ _ = handleArithmeticComparison expr1 expr2
getType (Infix Mult expr1 expr2) _ _ = handleArithmeticComparison expr1 expr2
getType (Infix Div expr1 expr2) _ _ = handleArithmeticComparison expr1 expr2

-- getType (Infix (Either Add (Either Sub (Either Mult Div))) _ expr1 expr2) _ _ =
--   if (all (=="Int") $ map getType [expr1, expr2]) then "Int" else "False"

getType (If condition then_expr else_expr) _ _ = 
   --TODO: función se llama dos veces y retornar error en vez de false
  if (getType then_expr) == "Bool" && (getType then_expr) == (getType else_expr)
  then (getType then_expr)
  else "False"

getType (Let (to_substitute_name, to_substitute_type) substituted_expr final_expression) env defs =
  if (getType substituted_expr env defs) == (getType to_substitute_type env defs) 
    && (getType to_substitute_name env defs) == (getType to_substitute_type env defs)
    -- TODO: Quitar ultima condicion? Agregar checkeo extra de si x tiene el mismo tipo que las x en e'.
  then (getType final_expression env defs)
  else "False"

getType (App name expressions) env defs = --TODO: Releer letra asquerosa
  if all (==True) $ zipWith (==) (map (\x -> getType x env defs) (getFunctionDefinitionByName name defs)) (map getType expressions)
  then getFunctionType $ getFunctionDefinitionByName name
  else "False"

-- TODO: agregar los checked
-- TODO: Hacer lets.
checkExpressionTypes :: Program -> Env -> Checked
checkExpressionTypes (Program defs expr) env = 
  map (\x -> getType x env defs) expr --TODO: Agregar errores

  

-- ########################################
-- ################# ALL ##################
-- ########################################

-- checkProgram :: Program -> Checked
-- checkProgram prog = 
--   if checkRepeatedNames prog == Wrong errors then Wrong errors else
--   if checkParamInts prog == Wrong errors then Wrong errors else
--   if checkUndefinedNames prog == Wrong errors then Wrong errors else
--   -- if checkExpressionTypes prog == Wrong errors then Wrong errors else
--   Ok

-- checkProgram :: Program -> Checked
-- checkProgram prog =
--   if errors1 /= [] then Wrong errors1 else
--   if errors2 /= [] then Wrong errors2 else
--   if errors3 /= [] then Wrong errors3 else
--   -- if errors4 != [] then Wrong errors4 else
--   Ok
--     where
--       errors1 = checkRepeatedNames prog
--       errors2 = checkParamInts prog
--       errors3 = checkUndefinedNames prog
--       -- errors4 = checkExpressionTypes prog


-- IMPORTANTE: Hay que hacer que cada checker devuelva una lista de errores
checkProgram :: Program -> Checked
checkProgram prog
  | errors1 /= [] = Wrong errors1
  | errors2 /= [] = Wrong errors2
  | errors3 /= [] = Wrong errors3
  -- | errors4 /= [] = Wrong errors4
  | otherwise = Ok
  where
    errors1 = checkRepeatedNames prog
    errors2 = checkParamInts prog
    errors3 = checkUndefinedNames prog
    -- errors4 = checkExpressionTypes prog
