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
checkRepeatedNames prog = if (length (checkFunctionDeclarations prog) == 0) && (length (checkParamsNames prog) == 0) then [] else (checkFunctionDeclarations prog ++ checkParamsNames prog)

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
checkUndefinedNames prog = if (length (checkUndefinedNamesFuncDecl prog) == 0) && (length (checkUndefinedNamesMain prog) == 0) then [] else (checkUndefinedNamesFuncDecl prog ++ checkUndefinedNamesMain prog)

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

-- Counts number of parameters from each function **signature**
getSignatureParamCounts :: Defs -> [(String, Int)] -> [(String, Int)]
getSignatureParamCounts [] results = results
getSignatureParamCounts ((FunDef (name, Sig signature_types _) _ _):xs) result = 
  getSignatureParamCounts xs ((name, length signature_types):result)

-- Counts number of parameters from each function **declaration** (definition)
getFunctionParamCounts :: Defs -> [(String, Int)] -> [(String, Int)]
getFunctionParamCounts [] results = results
getFunctionParamCounts ((FunDef (name, _) params _):xs) result = 
  getFunctionParamCounts xs ((name, length params):result)

-- Counts number of parameters from each function **expression** (application)
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
checkDictInconsistency :: (String -> Int -> Int -> Error) -> (String, Int) -> [(String, Int)] -> [Error] -> [Error]
checkDictInconsistency _ (key, value) [] errors = errors
checkDictInconsistency error_callback (key, value) ((other_key, other_value):xs) errors
  | key==other_key && value == other_value = checkDictInconsistency error_callback (key, value) xs errors
  | otherwise = checkDictInconsistency error_callback (key, value) xs ((error_callback key value other_value):errors)

checkDictInconsistencies :: (String -> Int -> Int -> Error) -> [(String, Int)] -> [(String, Int)] -> [Error]
checkDictInconsistencies error_callback tuples1 tuples2 = concatMap (\tuple -> checkDictInconsistency error_callback tuple tuples2 []) tuples1

checkParamInts :: Program -> [Error]
checkParamInts (Program defs expr) = errors
  where
    signatureParamCounts = getSignatureParamCounts defs []
    expressionParamCounts = getExpressionParamCounts expr
    functionParamCounts = getFunctionParamCounts defs []
    error_callback_definition :: String-> Int -> Int -> Error
    error_callback_definition n s d = (ArgNumDef n s d)
    error_callback_application :: String-> Int -> Int -> Error
    error_callback_application n s d = (ArgNumApp n s d)
    errors = 
      (checkDictInconsistencies (error_callback_definition) signatureParamCounts expressionParamCounts) ++
      (checkDictInconsistencies (error_callback_application) signatureParamCounts functionParamCounts)

-- ########################################
-- ############# CHECKER 2.4 ##############
-- ########################################

-- HELPERS-----------
getFunctionDefinitionByName :: String -> Defs -> FunDef
getFunctionDefinitionByName name defs = clean found_fundef
  where 
    found_fundef = find (\(FunDef (f_name, _) _ _) -> f_name == name) defs
    clean :: Maybe FunDef -> FunDef
    clean (Nothing) = FunDef ("", Sig [] TyBool) [] (BoolLit True)
    clean (Just x) = x

getFunctionType :: FunDef -> Type
getFunctionType (FunDef (_, (Sig _  function_type)) _ _) = function_type

getFunctionParamsType :: FunDef -> [Type]
getFunctionParamsType (FunDef (_, (Sig param_types  _)) _ _) = param_types

getFunctionParamNames :: FunDef -> [String]
getFunctionParamNames (FunDef _ param_names _) = param_names


handleExpressionComparison :: Expr -> Expr -> Env -> Defs -> String -> (String, [Error])
handleExpressionComparison expr1 expr2 env defs type_str = --TODO: Less than para bools??
  if type_expr1 == type_expr2
  then (type_str, subexpression_errors) 
  else (type_str, subexpression_errors ++ [getTypeError type_expr1 type_expr2])
  where
    type_and_errors_expr1 = getType (expr1) env defs
    type_and_errors_expr2 = getType (expr2) env defs
    type_expr1 = fst type_and_errors_expr1
    type_expr2 = fst type_and_errors_expr2
    subexpression_errors = (snd type_and_errors_expr1)++(snd type_and_errors_expr2)

handleArithmeticComparison :: Expr -> Expr -> Env -> Defs -> (String, [Error])
handleArithmeticComparison expr1 expr2 env defs = 
  ("Int", subexpression_errors ++ type_errors) --Si no hay errores se pasa vacío
  where
    types_and_errors = map (\expr -> getType expr env defs) [expr1, expr2]
    subexpression_errors = (snd $ types_and_errors !! 0) ++ (snd $ types_and_errors !! 1)
    subexpression_types = map fst types_and_errors
    non_integer_types = filter (/= "Int") subexpression_types
    type_errors = map (\curr_type -> getTypeError "Int" curr_type) non_integer_types

getVarTypeEnv :: String -> Env -> Type
getVarTypeEnv name env = snd $ clean found_name
  where 
    found_name = find (\name_type -> (fst name_type) == name) env
    clean (Nothing) = ("", TyBool)
    clean (Just x)  = x

getTypeError :: String -> String -> Error
getTypeError type1 type2 = Expected type1_type type2_type
  where
    getType :: String -> Type
    getType "Int" = TyInt
    getType "Bool" = TyBool
    type1_type = getType type1
    type2_type = getType type2
------------------

getTypeType :: Type -> String
getTypeType TyInt = "Int"
getTypeType TyBool = "Bool"

getType :: Expr -> Env -> Defs -> (String, [Error])
getType (Var name) env _ = ((getTypeType (getVarTypeEnv name env)), [])
getType (IntLit _) _ _ = ("Int", [])
getType (BoolLit _) _ _ = ("Bool", [])

getType (Infix Eq expr1 expr2) env defs =
  handleExpressionComparison expr1 expr2 env defs "Bool"
getType (Infix NEq expr1 expr2) env defs =
  handleExpressionComparison expr1 expr2 env defs "Bool"
getType (Infix GTh expr1 expr2) env defs =
  handleExpressionComparison expr1 expr2 env defs "Bool"
getType (Infix LTh expr1 expr2) env defs =
  handleExpressionComparison expr1 expr2 env defs "Bool"
getType (Infix GEq expr1 expr2) env defs =
  handleExpressionComparison expr1 expr2 env defs "Bool"
getType (Infix LEq expr1 expr2) env defs =
  handleExpressionComparison expr1 expr2 env defs "Bool"

-- getType (Infix (Either Eq (Either NEq (Either GTh (Either LTh (Either GEq LEq))))) _ expr1 expr2) env defs = --TODO: Less than para bools??
--   if ((getType expr1) == (getType expr2)) then "Bool" else 'error' --TODO: función se llama dos veces
--"Int" if (all (\x -> x `elem` ["Int", "Bool"]) $ map getType [expr1, expr2]) else False

getType (Infix Add expr1 expr2) env defs = handleArithmeticComparison expr1 expr2 env defs
getType (Infix Sub expr1 expr2) env defs = handleArithmeticComparison expr1 expr2 env defs
getType (Infix Mult expr1 expr2) env defs = handleArithmeticComparison expr1 expr2 env defs
getType (Infix Div expr1 expr2) env defs = handleArithmeticComparison expr1 expr2 env defs

-- getType (Infix (Either Add (Either Sub (Either Mult Div))) _ expr1 expr2) _ _ =
--   if (all (=="Int") $ map getType [expr1, expr2]) then "Int" else "False"

getType (If condition then_expr else_expr) env defs = 
  (returned_type, curr_returned_expression_error ++ curr_boolean_condition_error ++ subexpression_errors)
  where
    type_and_error_condition = getType condition env defs
    type_and_error_then = getType then_expr env defs
    type_and_error_else = getType else_expr env defs
    returned_type = fst type_and_error_else
    subexpression_errors = (snd type_and_error_condition) ++ (snd type_and_error_then) ++ (snd type_and_error_else)
    condition_type = fst type_and_error_condition
    then_type = fst type_and_error_then
    else_type = fst type_and_error_else
    is_boolean_condition = condition_type == "Bool"
    are_equal_return_expressions = then_type == else_type
    curr_boolean_condition_error = if is_boolean_condition then [] else [getTypeError "Bool" condition_type]
    curr_returned_expression_error = if are_equal_return_expressions then [] else [getTypeError else_type then_type]

-- TODO: Quitar ultima condicion? Agregar checkeo extra de si x tiene el mismo tipo que las x en e'.
getType (Let (to_substitute_name, to_substitute_type) substituted_expr final_expression) env defs =
  (type_final_expression, curr_var_to_substitute_type_error ++ curr_substituted_to_substitute_type_error)
  where
    final_expression_env = (to_substitute_name, to_substitute_type):env
    type_and_error_substituted = getType substituted_expr env defs
    type_and_error_var = getType (Var to_substitute_name) env defs
    type_and_error_final_expression = getType final_expression final_expression_env defs
    type_type_to_substitute = getTypeType to_substitute_type
    type_substituted = fst type_and_error_substituted
    type_var = fst type_and_error_var
    type_final_expression = fst type_and_error_final_expression
    are_equal_type_substituted_to_substitute = type_substituted == type_type_to_substitute
    are_equal_type_var_to_substitute = type_var == type_type_to_substitute
    curr_substituted_to_substitute_type_error = if are_equal_type_substituted_to_substitute then [] else [getTypeError type_type_to_substitute type_substituted]
    curr_var_to_substitute_type_error = if are_equal_type_var_to_substitute then [] else [getTypeError type_type_to_substitute type_var]

getType (App name expressions) env defs = --TODO: Releer letra
  (function_type, current_errors)
    where 
      functionDef = getFunctionDefinitionByName name defs
      type_and_error_env = map (\expr -> getType expr env defs) expressions
      function_type = getTypeType $ getFunctionType functionDef
      para_types_definition = map getTypeType (getFunctionParamsType functionDef)
      param_types_expression = map fst type_and_error_env
      check_error :: String -> String -> [Error]
      check_error type1 type2 = if type1 /= type2 then [getTypeError type1 type2] else []
      current_errors = concat $ filter (\x -> length x > 0) $ zipWith check_error para_types_definition param_types_expression

-- TODO: agregar los checked
checkExpressionTypes :: Program -> [Error]
checkExpressionTypes (Program defs expr) = errors
  where
    -- [
    --   [x,y,z]
    --   [x,y]
    -- ]
    -- [
    --    [int, int, bool],
    --    [char, char]
    -- ]
    -- envs_params_types_str :: [[String]]
    -- envs_params_types_str = map (\x -> map getTypeType x) envs_params_types
    -- [[(x, int), (y, int), (z, bool)], [(x, char), (y, char)]]
    -- [f1, f2, f3]
    -- zip -> [(e1, f1), (e2, f2), (e3, f3)]
    -- map -> getType zipeado -> (type, [Error])
    envs_params_names :: [[String]]
    envs_params_names = map getFunctionParamNames defs
    envs_params_types :: [[Type]]
    envs_params_types = map getFunctionParamsType defs
    envs :: [Env]
    envs = zipWith zip envs_params_names envs_params_types -- ver tipo de type
    envs_and_defs :: [(Env, Defs)]
    envs_and_defs = zip envs (map (\x -> [x]) defs)
    types_and_expressions_main = getType expr [] defs -- TODO: Ver env=[]
    types_and_expressions_functions = map (\(curr_env, fundef) -> getType expr curr_env fundef) envs_and_defs
    errors = (snd types_and_expressions_main) ++ (concatMap snd types_and_expressions_functions)

-- ########################################
-- ################# ALL ##################
-- ########################################

-- checkProgram :: Program -> Checked
-- checkProgram prog = Ok
  -- if checkRepeatedNames prog == Wrong errors then Wrong errors else
  -- if checkParamInts prog == Wrong errors then Wrong errors else
  -- if checkUndefinedNames prog == Wrong errors then Wrong errors else
  -- -- if checkExpressionTypes prog == Wrong errors then Wrong errors else
  -- Ok

-- checkProgram' :: Program -> Checked
-- checkProgram' prog =
--   if length errors1 != 0 then Wrong errors1 else
--   if length errors2 != 0 then Wrong errors2 else
--   if length errors3 != 0 then Wrong errors3 else
--   -- if errors4 != [] then Wrong errors4 else
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


-- -- IMPORTANTE: Hay que hacer que cada checker devuelva una lista de errores
checkProgram :: Program -> Checked
checkProgram prog
  | length errors1 /= 0 = Wrong errors1
  | length errors2 /= 0 = Wrong errors2
  | length errors3 /= 0 = Wrong errors3
  | length errors4 /= 0 = Wrong errors4
  | otherwise = Ok
  where
    errors1 = checkRepeatedNames prog
    errors2 = checkParamInts prog
    errors3 = checkUndefinedNames prog
    errors4 = checkExpressionTypes prog
