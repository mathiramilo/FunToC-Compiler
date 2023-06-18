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
  | (key==other_key && value /= other_value) = checkDictInconsistency error_callback (key, value) xs ((error_callback key value other_value):errors)
  | otherwise = checkDictInconsistency error_callback (key, value) xs errors

checkDictInconsistencies :: (String -> Int -> Int -> Error) -> [(String, Int)] -> [(String, Int)] -> [Error]
checkDictInconsistencies error_callback tuples1 tuples2 = concatMap (\tuple -> checkDictInconsistency error_callback tuple tuples2 []) tuples1

checkParamInts :: Program -> [Error]
checkParamInts (Program defs expr) = errors
  where
    signatureParamCounts = getSignatureParamCounts defs []
    functionParamCounts = getFunctionParamCounts defs []
    error_callback_definition :: String-> Int -> Int -> Error
    error_callback_definition n s d = (ArgNumDef n s d)
    errors = (checkDictInconsistencies error_callback_definition signatureParamCounts functionParamCounts)

-- ########################################
-- ############# CHECKER 2.4 ##############
-- ########################################

-- HELPERS----------------------------------------------------------
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

getFunctionExpression :: FunDef -> Expr
getFunctionExpression (FunDef _ _ expr) = expr

handleExpressionComparison :: ([(String, Int)] -> [Error]) -> Expr -> Expr -> Env -> Defs -> String -> (String, [Error])
handleExpressionComparison error_callback_application expr1 expr2 env defs type_str = --TODO: Less than para bools??
  if type_expr1 == type_expr2
  then (type_str, subexpression_errors) 
  else (type_str, [getTypeError type_expr1 type_expr2] ++ subexpression_errors)
  where
    type_and_errors_expr1 = getType error_callback_application (expr1) env defs
    type_and_errors_expr2 = getType error_callback_application (expr2) env defs
    type_expr1 = fst type_and_errors_expr1
    type_expr2 = fst type_and_errors_expr2
    subexpression_errors = (snd type_and_errors_expr1)++(snd type_and_errors_expr2)

handleArithmeticComparison :: ([(String, Int)] -> [Error]) -> Expr -> Expr -> Env -> Defs -> (String, [Error])
handleArithmeticComparison error_callback_application expr1 expr2 env defs = 
  ("Int", type_errors ++ subexpression_errors) --Si no hay errores se pasa vacío  ++ [Duplicated (show types_and_errors)]
  where
    types_and_errors = map (\expr -> getType error_callback_application expr env defs) [expr1, expr2]
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
    getType _ = TyBool

    type1_type = getType type1
    type2_type = getType type2
----------------------------------------------------------------------------

getTypeType :: Type -> String
getTypeType TyInt = "Int"
getTypeType TyBool = "Bool"

getType :: ([(String, Int)] -> [Error]) -> Expr -> Env -> Defs -> (String, [Error])
getType error_callback_application (Var name) env _ = ((getTypeType (getVarTypeEnv name env)), [])
getType error_callback_application (IntLit _) _ _ = ("Int", [])
getType error_callback_application (BoolLit _) _ _ = ("Bool", [])

getType error_callback_application (Infix Eq expr1 expr2) env defs =
  handleExpressionComparison error_callback_application expr1 expr2 env defs "Bool"
getType error_callback_application (Infix NEq expr1 expr2) env defs =
  handleExpressionComparison error_callback_application expr1 expr2 env defs "Bool"
getType error_callback_application (Infix GTh expr1 expr2) env defs =
  handleExpressionComparison error_callback_application expr1 expr2 env defs "Bool"
getType error_callback_application (Infix LTh expr1 expr2) env defs =
  handleExpressionComparison error_callback_application expr1 expr2 env defs "Bool"
getType error_callback_application (Infix GEq expr1 expr2) env defs =
  handleExpressionComparison error_callback_application expr1 expr2 env defs "Bool"
getType error_callback_application (Infix LEq expr1 expr2) env defs =
  handleExpressionComparison error_callback_application expr1 expr2 env defs "Bool"

-- getType (Infix (Either Eq (Either NEq (Either GTh (Either LTh (Either GEq LEq))))) _ expr1 expr2) env defs = --TODO: Less than para bools??
--   if ((getType expr1) == (getType expr2)) then "Bool" else 'error' --TODO: función se llama dos veces
--"Int" if (all (\x -> x `elem` ["Int", "Bool"]) $ map getType [expr1, expr2]) else False

getType error_callback_application (Infix Add expr1 expr2) env defs = 
  handleArithmeticComparison error_callback_application expr1 expr2 env defs
getType error_callback_application (Infix Sub expr1 expr2) env defs = 
  handleArithmeticComparison error_callback_application expr1 expr2 env defs
getType error_callback_application (Infix Mult expr1 expr2) env defs = 
  handleArithmeticComparison error_callback_application expr1 expr2 env defs
getType error_callback_application (Infix Div expr1 expr2) env defs = 
  handleArithmeticComparison error_callback_application expr1 expr2 env defs

-- getType (Infix (Either Add (Either Sub (Either Mult Div))) _ expr1 expr2) _ _ =
--   if (all (=="Int") $ map getType [expr1, expr2]) then "Int" else "False"

getType error_callback_application (If condition then_expr else_expr) env defs = 
  (returned_type, curr_boolean_condition_error ++ curr_returned_expression_error ++ subexpression_errors)
  where
    type_and_error_condition = getType error_callback_application condition env defs
    type_and_error_then = getType error_callback_application then_expr env defs
    type_and_error_else = getType error_callback_application else_expr env defs
    returned_type = fst type_and_error_then
    subexpression_errors = (snd type_and_error_condition) ++ (snd type_and_error_then) ++ (snd type_and_error_else)
    condition_type = fst type_and_error_condition
    then_type = fst type_and_error_then
    else_type = fst type_and_error_else
    is_boolean_condition = condition_type == "Bool"
    are_equal_return_expressions = then_type == else_type
    curr_boolean_condition_error = if is_boolean_condition then [] else [getTypeError "Bool" condition_type]
    curr_returned_expression_error = if are_equal_return_expressions then [] else [getTypeError then_type else_type]

-- TODO: Quitar ultima condicion? Agregar checkeo extra de si x tiene el mismo tipo que las x en e'.
getType error_callback_application (Let (to_substitute_name, to_substitute_type) substituted_expr final_expression) env defs =
  (type_final_expression, curr_substituted_to_substitute_type_error ++ errors_final_expression)
  where
    env_without_substituted_variable = filter (\(name, _) -> name /= to_substitute_name) env

    -- Get type in e' --> e'[x::t/e] <--
    final_expression_env = (to_substitute_name, to_substitute_type):env_without_substituted_variable
    type_and_errors_final_expression = getType error_callback_application final_expression final_expression_env defs
    type_final_expression = fst type_and_errors_final_expression --FALTA SND
    errors_final_expression = snd type_and_errors_final_expression

    -- Compare types t ^ e --> e'[x::t/e] <-- (all (noLigado(x) and type(x)==t) types(x in t))
    type_and_error_substituted = getType error_callback_application substituted_expr final_expression_env defs
    type_substituted = fst type_and_error_substituted
    type_type_to_substitute = getTypeType to_substitute_type
    are_equal_type_substituted_to_substitute = type_substituted == type_type_to_substitute
    curr_substituted_to_substitute_type_error = if are_equal_type_substituted_to_substitute then [] else [getTypeError type_type_to_substitute type_substituted]

    -- Compare types t ^ x --> e'[x::t/e] <-- (all (noLigado(x) and type(x)==t) types(x in t))
    -- TYPE OF T = to_substitute_type
    -- TYPE OF X = ?.
    -- ...
    -- Type in x --> e'[x::t/e] <-- BORRAR BORRAR BORRAR
    -- type_and_error_var = getType error_callback_application (Var to_substitute_name) env defs -- MAL. No tiene sentido checkear errores en una variable.
    -- type_var = fst type_and_error_var -- Mal (error propagado)
    -- are_equal_type_var_to_substitute = type_var == type_type_to_substitute --eh?
    -- curr_var_to_substitute_type_error = if are_equal_type_var_to_substitute then [] else [getTypeError type_type_to_substitute type_var]



getType error_callback_application (App name expressions) env defs = --TODO: Releer letra
  (function_type,argument_count_errors ++ current_parameter_type_errors ++ errors_subexpressions) -- TODO: Ver orden
    where
      -- Checks inconsistency in number of parameters of application-definition
      argument_count_errors = error_callback_application [(name, length expressions)]

      -- Checks subexpressions' types and errors
      functionDef = getFunctionDefinitionByName name defs
      type_and_error_env = map (\expr -> getType error_callback_application expr env defs) expressions --TODO: Por que no concateno estos errores???
      function_type = getTypeType $ getFunctionType functionDef
      para_types_definition = map getTypeType (getFunctionParamsType functionDef)
      param_types_expression = map fst type_and_error_env
      errors_subexpressions = concatMap snd type_and_error_env

      -- Checks each parameter 1 by 1 against definition ([bool, bool, bool] | [True, True, 2]) -> Expected Bool. Actual Int
      check_error :: String -> String -> [Error]
      check_error type1 type2 = if type1 /= type2 then [getTypeError type1 type2] else []
      current_parameter_type_errors = concat $ filter (\x -> length x > 0) $ zipWith check_error para_types_definition param_types_expression

checkExpressionTypes :: Program -> [Error]
checkExpressionTypes (Program defs expr) =  errors_functions ++ errors_main
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

    -- Loads environments for function definitions
    envs_params_names :: [[String]] --TODO: Env debe crearse o se llena dinámicamente????
    envs_params_names = map getFunctionParamNames defs
    envs_params_types :: [[Type]]
    envs_params_types = map getFunctionParamsType defs
    envs :: [Env]
    envs = zipWith zip envs_params_names envs_params_types -- ver tipo de type
    envs_and_defs :: [(Env, FunDef)]
    envs_and_defs = zip envs defs --TODO: y este reshape?

    -- Auxiliar callbacks to return errors given the total parameters of a function
    error_application :: String-> Int -> Int -> Error
    error_application n s d = (ArgNumApp n s d)
    signatureParamCounts = getSignatureParamCounts defs []
    expressionParamCounts = getExpressionParamCounts expr
    error_callback_application :: [(String, Int)] -> [Error]
    error_callback_application params = checkDictInconsistencies error_application signatureParamCounts params
      
    -- Checks for errors in function return types
    -- Checks for errors in function expressions
    types_and_errors_functions = map (\(curr_env, fundef) -> getType error_callback_application (getFunctionExpression fundef) curr_env defs) envs_and_defs
    functions_return_type = map getTypeType $ map getFunctionType defs
    typeserrors_and_correcttypes_functions = zip types_and_errors_functions functions_return_type
    errors_functions = (concatMap (\((curr_type, curr_errors), correct_type) -> curr_errors ++ (if curr_type == correct_type then [] else [getTypeError correct_type curr_type])) typeserrors_and_correcttypes_functions) -- ++ [Duplicated (show expr), Duplicated (show $ fst $ envs_and_defs !! 0), Duplicated (show $ snd $ envs_and_defs !! 0)]
      -- concatMap (\fun_expr -> getType error_callback_application fun_expr [] defs)
      -- map (\(curr_env, fundef) -> getType error_callback_application expr curr_env fundef) envs_and_defs --TODO: necesario??

    -- Checks for errors in main expressions
    type_and_errors_main = getType error_callback_application expr [] defs -- TODO: Ver env=[]
    errors_main = (snd type_and_errors_main) -- ++ [Duplicated (show $ types_and_expressions_main)] -- ++[Duplicated (show expr), Duplicated (show defs), Duplicated (show types_and_expressions_main)]

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
  | length errors2 /= 0 = Wrong (errors2)-- ++ [Duplicated (show errors2)])
  | length errors3 /= 0 = Wrong (errors3)-- ++ [Duplicated (show errors3)])
  | length errors4 /= 0 = Wrong (errors4)-- ++ [])--getTypeError "Hola" "Adios"])
  | otherwise = Ok
  where
    errors1 = checkRepeatedNames prog
    errors2 = if length errors1 == 0 then (checkParamInts prog) else []
    errors3 = if length errors2 == 0 then (checkUndefinedNames prog) else []
    errors4 = if length errors3 == 0 then (checkExpressionTypes prog) else []