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

import Data.List

-- CODE GENERATOR

-- En general quedo pronta la generacion de codigo C, falta revisar que la transformacion sea tal cual se pide para que pase los tests
genProgram :: Program -> String
genProgram (Program defs expr) =
  let functionDecls = genFunctionDecls defs
      mainFunction = genExpr expr
  in "#include<stdio.h>\n" ++ functionDecls ++ "\nint main() {\n" ++ "printf(\"%d\\n\"," ++ mainFunction ++ "); }\n"

-- Funcion encargada de generar el codigo C de las declaraciones de funciones
genFunctionDecls :: [FunDef] -> String
genFunctionDecls defs = intercalate "\n" (map genFunctionDecl defs)

genFunctionDecl :: FunDef -> String
genFunctionDecl (FunDef (name, sig) params expr) =
  let returnType = genType (sigReturnType sig)
      paramList = genParams params
      functionHeader = returnType ++ " " ++ "_" ++ name ++ "(" ++ paramList ++ ")"
      functionBody = genExpr expr
  in functionHeader ++ "{\n" ++ functionBody ++ "};\n"
  where
    genParams :: [Name] -> String
    genParams [] = ""
    genParams [x] = genParam x
    genParams (x:xs) = genParam x ++ ", " ++ genParams xs

    genParam :: Name -> String
    genParam name = "_" ++ name

    sigReturnType :: Sig -> Type
    sigReturnType (Sig _ returnType) = returnType

-- Funcion encargada de generar el codigo C de una expresion
genExpr :: Expr -> String
genExpr (Var name) = "_" ++ name
genExpr (IntLit n) = show n
genExpr (BoolLit b) = if b then "1" else "0"
genExpr (Infix op e1 e2) =
  let opStr = genOp op
      arg1 = genExpr e1
      arg2 = genExpr e2
  in "(" ++ arg1 ++ " " ++ opStr ++ " " ++ arg2 ++ ")"
genExpr (If cond e1 e2) =
  let condition = genExpr cond
      thenExpr = genExpr e1
      elseExpr = genExpr e2
  in "if (" ++ condition ++ ") {\n" ++ thenExpr ++ "\n} else {\n" ++ elseExpr ++ "\n}"
-- REVISAR: La transformacion de let a C no es de esta manera
genExpr (Let (name, typ) e1 e2) = 
  let varType = genType typ
      varName = "_" ++ name
      expr1 = genExpr e1
      expr2 = genExpr e2
  in varType ++ " " ++ varName ++ " = " ++ expr1 ++ ";\n" ++ expr2
genExpr (App name args) =
  let argList = genArgs args
  in "_" ++ name ++ "(" ++ argList ++ ")"
  where
    genArgs :: [Expr] -> String
    genArgs [] = ""
    genArgs [x] = genExpr x
    genArgs (x:xs) = genExpr x ++ ", " ++ genArgs xs

genType :: Type -> String
genType TyInt = "int"
genType TyBool = "int"

genOp :: Op -> String
genOp Add = "+"
genOp Sub = "-"
genOp Mult = "*"
genOp Div = "/"
genOp Eq = "=="
genOp NEq = "!="
genOp GTh = ">"
genOp LTh = "<"
genOp GEq = ">="
genOp LEq = "<="
