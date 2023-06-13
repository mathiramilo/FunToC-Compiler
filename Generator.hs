----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de generación de código C
--
-- Se debe implementar la función genProgram,
-- que dado un AST que representa un programa válido
-- genera el código C correspondiente.
----------------------------------------------------------------------------

module Generator where

import Syntax
-- se pueden agregar mas importaciones 
-- en caso de ser necesario
import Data.List (intercalate)

-- CODE GENERATOR

-- En general quedo pronta la generacion de codigo C, falta revisar que la transformacion sea tal cual se pide para que pase los tests
genProgram :: Program -> String
genProgram (Program defs expr) =
  let functionDecls = genFunctionDecls defs
      mainFunctionLets = genMainExprLets expr
      mainFunctionBdy = genMainExprBdy expr
  in "#include <stdio.h>\n" ++ functionDecls ++ "\nint main() {\n" ++ mainFunctionLets ++ "printf(\"%d\\n\"," ++ mainFunctionBdy ++ "); }\n"

-- Funcion encargada de generar el codigo C de la expresion principal
genMainExprBdy :: Expr -> String
genMainExprBdy expr = undefined

genMainExprLets :: Expr -> String
genMainExprLets expr = undefined

-- Funcion encargada de generar el codigo C de las declaraciones de funciones
genFunctionDecls :: [FunDef] -> String
genFunctionDecls defs = intercalate "\n" (map genFunctionDecl defs)

genFunctionDecl :: FunDef -> String
genFunctionDecl (FunDef (name, sig) params expr) =
  let returnType = genType (sigReturnType sig)
      paramList = genParams params
      functionHeader = returnType ++ " " ++ "_" ++ name ++ "(" ++ paramList ++ ")"
      (functionLets, functionBody) = genExpr expr
  in functionHeader ++ "{\n" ++ functionLets ++ functionBody ++ "};\n"
  where
    genParams :: [Name] -> String
    genParams [] = ""
    genParams [x] = genParam x
    genParams (x:xs) = genParam x ++ "," ++ genParams xs

    genParam :: Name -> String
    genParam name = "_" ++ name

    sigReturnType :: Sig -> Type
    sigReturnType (Sig _ returnType) = returnType

    -- Funcion encargada de generar el codigo C de una expresion
    genExpr :: Expr -> (String, String)
    genExpr (Var name) = ("_" ++ name, "")
    genExpr (IntLit n) = (show n, "")
    genExpr (BoolLit b) = (if b then "1" else "0", "")
    genExpr (Infix op e1 e2) =
      let opStr = genOp op
          (arg1, _) = genExpr e1
          (arg2, _) = genExpr e2
      in ("(" ++ arg1 ++ opStr ++ arg2 ++ ")", "")
    genExpr (If cond e1 e2) =
      let (condition, _) = genExpr cond
          (thenExpr, _) = genExpr e1
          (elseExpr, _) = genExpr e2
      in (condition ++ "?(" ++ thenExpr ++ "):(" ++ elseExpr ++ ")", "")
    -- REVISAR: La transformacion de let a C no es de esta manera
    genExpr (Let (name, typ) e1 e2) = 
      let varType = genType typ
          varName = "_let" ++ show randomInt
          paramName = "_" ++ name
          (expr1, _) = genExpr e1
          (expr2, _) = genExpr e2
      in (varName ++ "(" ++ expr1 ++ ")", varType ++ " " ++ varName ++ "(" ++ varType ++ paramName ++ "){\n" ++ "return (" ++ genExpr expr2 ++ "); };")
    genExpr (App name args) =
      let argList = genArgs args
      in ("_" ++ name ++ "(" ++ argList ++ ")", "")
      where
        genArgs :: [Expr] -> String
        genArgs [] = ""
        genArgs [x] = genExpr x
        genArgs (x:xs) = genExpr x ++ "," ++ genArgs xs

    -- Funcion encargada de generar el codigo C de las declaraciones de lets
    genExprLets :: Expr -> Int -> String
    genExprLets (Var name) _ = ""
    genExprLets (IntLit n) _ = ""
    genExprLets (BoolLit b) _ = ""
    genExprLets (Infix op e1 e2) count = genExprLets e1 count ++ genExprLets e2 count
    genExprLets (If cond e1 e2) count = genExprLets cond count ++ genExprLets e1 count ++ genExprLets e2 count
    genExprLets (Let (name, typ) e1 e2) count =
      let varType = genType typ
          varName = "_let" ++ show count
          paramName = "_" ++ name
          expr1 = genExpr e1 count
          expr2 = genExpr e2 count
      in varType ++ " " ++ varName ++ "(" ++ varType ++ paramName ++ "){\n" ++ "return (" ++ genExprLets e2 count ++ "); };"
    genExprLets (App name args) count = genExprLetsArgs args
      where
        genExprLetsArgs :: [Expr] -> String
        genExprLetsArgs [] = ""
        genExprLetsArgs [x] = genExprLets x count
        genExprLetsArgs (x:xs) = genExprLets x count ++ genExprLetsArgs xs

genType :: Type -> String
genType TyInt = "int"
genType TyBool = "int"

genOp :: Op -> String
genOp Add = " + "
genOp Sub = " - "
genOp Mult = " * "
genOp Div = " / "
genOp Eq = "=="
genOp NEq = "!="
genOp GTh = ">"
genOp LTh = "<"
genOp GEq = ">="
genOp LEq = "<="

-- TODO: Funcion encargada de generar un numero aleatorio
randomInt :: Int
randomInt = 0
