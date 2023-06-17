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
import Data.ByteString (count)

-- CODE GENERATOR

-- En general quedo pronta la generacion de codigo C, falta revisar que la transformacion sea tal cual se pide para que pase los tests
genProgram :: Program -> String
genProgram (Program defs expr) =
  let functionDecls = genFunctionDecls defs
      mainExpr = genMainExpr expr
  in "#include <stdio.h>\n" ++ functionDecls ++ mainExpr


-- Funcion encargada de generar el codigo C de las declaraciones de funciones
genFunctionDecls :: [FunDef] -> String
genFunctionDecls defs = intercalate "\n" (map genFunctionDecl defs)

genFunctionDecl :: FunDef -> String
genFunctionDecl (FunDef (name, sig) params expr) =
  let returnType = genType (sigReturnType sig)
      paramList = genParams params
      functionHeader = returnType ++ " " ++ "_" ++ name ++ "(" ++ paramList ++ ")"
      (body, lets, _) = genExpr expr 0
  in functionHeader ++ "{\n" ++ lets ++ "return (" ++ body ++ "); };"
  where
    genParams :: [Name] -> String
    genParams [] = ""
    genParams [x] = genParam x
    genParams (x:xs) = genParam x ++ "," ++ genParams xs

    genParam :: Name -> String
    genParam name = "int _" ++ name

    sigReturnType :: Sig -> Type
    sigReturnType (Sig _ returnType) = returnType


-- Funcion encargada de generar el codigo C de la expresion principal
genMainExpr :: Expr -> String
genMainExpr expr =
  let (body, lets, _) = genExpr expr 0
  in "\nint main() {\n" ++ lets ++ "printf(\"%d\\n\"," ++ body ++ "); }\n"


-- Funcion encargada de generar el codigo C de una expresion, retorna una tupla con el cuerpo de la expresion, las declaraciones de lets y la cantidad de lets que se han generado
genExpr :: Expr -> Int -> (String, String, Int)

genExpr (Var name) count = ("_" ++ name, "", count)
genExpr (IntLit n) count = (show n, "", count)
genExpr (BoolLit b) count = (if b then "1" else "0", "", count)

genExpr (Infix op e1 e2) count =
  let opStr = genOp op
      (expr1Bdy, expr1Lets, letsQty1) = genExpr e1 count
      (expr2Bdy, expr2Lets, letsQty2) = genExpr e2 letsQty1
  in ("(" ++ expr1Bdy ++ opStr ++ expr2Bdy ++ ")", expr1Lets ++ expr2Lets, letsQty2)

genExpr (If cond e1 e2) count =
  let (condBdy, condLets, letsQty1) = genExpr cond count
      (thenBdy, thenLets, letsQty2) = genExpr e1 letsQty1
      (elseBdy, elseLets, letsQty3) = genExpr e2 letsQty2
  in (condBdy ++ "?(" ++ thenBdy ++ "):(" ++ elseBdy ++ ")", condLets ++ thenLets ++ elseLets, letsQty3)

genExpr (Let (name, typ) e1 e2) count = 
  let varType = genType typ
      varName = "_let" ++ show count
      paramName = " _" ++ name
      (expr1Bdy, expr1Lets, letsQty1) = genExpr e1 (count + 1)
      (expr2Bdy, expr2Lets, letsQty2) = genExpr e2 letsQty1
  in (varName ++ "(" ++ expr1Bdy ++ ")", varType ++ " " ++ varName ++ "(" ++ varType ++ paramName ++ "){\n" ++ "return (" ++ expr2Bdy ++ "); };\n" ++ expr1Lets ++ expr2Lets, letsQty2)

genExpr (App name args) count =
  let (argList, argLets, letsQty) = genArgs args count
  in ("_" ++ name ++ "(" ++ argList ++ ")", argLets, letsQty)
  where
    genArgs :: [Expr] -> Int -> (String, String, Int)
    genArgs [] letsCount = ("", "", letsCount)
    genArgs [x] letsCount = genExpr x letsCount
    genArgs (x:xs) letsCount = (argBdy1 ++ "," ++ argBdy2, argLets1 ++ argLets2, letsQty2)
      where
        (argBdy1, argLets1, letsQty1) = genExpr x letsCount
        (argBdy2, argLets2, letsQty2) = genArgs xs letsQty1


-- Funcion encargada de generar el codigo C de un tipo
genType :: Type -> String
genType TyInt = "int"
genType TyBool = "int"

-- Funcion encargada de generar el codigo C de un operador
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
