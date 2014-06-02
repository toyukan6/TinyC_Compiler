module Syntax.AST where

data CVal = Atom String
	  | Number Integer
	  | Minus CVal
	  | Variation String
	  | NullExp ()
	  | List [CVal]
	  | CalFunc String [CVal]
	  | Expression String CVal CVal
	  | CompoundStatement [CVal] [CVal]

showVal :: CVal -> String
showVal (Atom name) = name
showVal (Variation name) = name
showVal (NullExp _) = ""
showVal (Number n) = show n
showVal (Minus n) = '-' : show n
showVal (List l) = unwordsList l
showVal (CalFunc ident var) = "(" ++ ident ++ " (" ++ unwordsList var ++ "))"
showVal (Expression op n1 n2) = "(" ++ op ++ " " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
showVal (CompoundStatement var exp) = "((" ++ unwordsList var ++ ")(" ++ unwordsList exp ++ "))"

instance Show CVal where show = showVal

unwordsList :: [CVal] -> String
unwordsList l = unwords . map show $ l
