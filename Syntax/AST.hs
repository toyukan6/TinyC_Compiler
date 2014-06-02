module Syntax.AST where

import Syntax.Type

data CVal = Atom String
	  | Number Integer
	  | Minus CVal
	  | Variation Type String
	  | NullExp ()
	  | List [CVal]
	  | CalFunc String [CVal]
	  | Assign CVal CVal
	  | Add CVal CVal
	  | Sub CVal CVal
	  | Mul CVal CVal
	  | Div CVal CVal
	  | Mod CVal CVal
	  | More CVal CVal
	  | Less CVal CVal
	  | MoreE CVal CVal
	  | LessE CVal CVal
	  | Equal CVal CVal
	  | NEqual CVal CVal
	  | L_AND CVal CVal
	  | L_OR CVal CVal
	  | CompoundStatement [CVal] [CVal]

showExpr :: String -> CVal -> CVal -> String
showExpr s c1 c2 = "(" ++ s ++ " " ++ show c1 ++ " " ++ show c2 ++ ")"

showVal :: CVal -> String
showVal (Atom name) = name
showVal (Variation Int name) = "(int " ++ name ++ ")"
showVal (NullExp _) = ""
showVal (Number n) = show n
showVal (Minus n) = '-' : show n
showVal (List l) = unwordsList l
showVal (CalFunc ident var) = "(" ++ ident ++ " (" ++ unwordsList var ++ "))"
showVal (Assign n1 n2) = showExpr "=" n1 n2
showVal (Add n1 n2) = showExpr "+" n1 n2
showVal (Sub n1 n2) = showExpr "-" n1 n2
showVal (Mul n1 n2) = showExpr "*" n1 n2
showVal (Div n1 n2) = showExpr "/" n1 n2
showVal (Mod n1 n2) = showExpr "%" n1 n2
showVal (More n1 n2) = showExpr "<" n1 n2
showVal (Less n1 n2) = showExpr ">" n1 n2
showVal (MoreE n1 n2) = showExpr "<=" n1 n2
showVal (LessE n1 n2) = showExpr ">=" n1 n2
showVal (Equal n1 n2) = showExpr "==" n1 n2
showVal (NEqual n1 n2) = showExpr "!=" n1 n2
showVal (L_AND n1 n2) = showExpr "and" n1 n2
showVal (L_OR n1 n2) = showExpr "or" n1 n2
showVal (CompoundStatement var exp) = "((" ++ unwordsList var ++ ")(" ++ unwordsList exp ++ "))"

instance Show CVal where show = showVal

unwordsList :: [CVal] -> String
unwordsList l = unwords . map show $ l
