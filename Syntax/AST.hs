module Syntax.AST where

import Data.Maybe

import Syntax.Type

--識別子の型
data Identifier = Identifier String

instance Show Identifier where
    show (Identifier s) = s

--パース結果の型
data CVal = Number Integer
	  | Ident Identifier
	  | Minus CVal
	  | CValList CVal CVal
	  | CalFunc Identifier [CVal]
	  | Assign Identifier CVal
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

--変数の型
data Variation = Variation Type Identifier

instance Show Variation where
    show (Variation t ident) = "(" ++ show t ++ " " ++ show ident ++ ")"

--statementのパース結果	  
data Statement = NullExp
               | Expression CVal
	       | If CVal Statement Statement
	       | While CVal Statement
               | For (Maybe CVal) (Maybe CVal) (Maybe CVal) Statement
	       | Return (Maybe CVal)
	       | Declaration [Variation]
               | CompoundStatement [Statement]

--パラメータ宣言の型
data ParamDecl = ParamDecl [Variation]
instance Show ParamDecl where
    show (ParamDecl var) = unwordsList var

--関数の型    
data Function = Func Type Identifier ParamDecl Statement

--プログラムの型
data Program = PDecl Statement
             | PFunc Function

instance Show Program where
    show (PDecl state) = showStatement state ""
    show (PFunc func) = show func

unwordsList :: (Show a) => [a] -> String
unwordsList l = unwords . map show $ l

indent :: (Show a) => [a] -> String
indent l = unwords . map ((++) "    " . show) $ l
    
showExpr :: String -> CVal -> CVal -> String
showExpr s c1 c2 = "(" ++ s ++ " " ++ show c1 ++ " " ++ show c2 ++ ")"

showVal :: CVal -> String
showVal (Number n) = show n
showVal (Ident ident) = show ident
showVal (Minus n) = '-' : show n
showVal (CValList l1 l2) = show l1 ++ show l2
showVal (CalFunc ident var) = "(" ++ show ident ++ " (" ++ unwordsList var ++ "))"
showVal (Assign n1 n2) = "(= " ++ show n1 ++ " " ++ show n2 ++ ")"
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

instance Show CVal where show = showVal

showStatement :: Statement -> String -> String
showStatement NullExp str = str ++ "()"
showStatement (Expression val) str = str ++ show val
showStatement (If cond state1 state2) str =
    str ++ "(if " ++ show cond ++ "\n" ++ showStatement state1 (str ++ "    ") ++ "\n"
            ++ showStatement state2 (str ++ "    ") ++ ")"
showStatement (While cond state) str =
    str ++ "(while " ++ show cond ++ showStatement state (str ++ "    ") ++ ")"
showStatement (For init cond update state) str =
    str ++ "(for " ++ (unwords . map show . catMaybes $ [init, cond, update])
            ++ showStatement state (str ++ "    ") ++ ")"
showStatement (Return (Just state)) str = str ++ "(return " ++ show state ++ ")"
showStatement (Return Nothing) str = str ++ "(return)"
showStatement (Declaration var) str = str ++ unwordsList var
showStatement (CompoundStatement state) str =
    "(\n" ++ init (unlines (map (`showStatement` str) state)) ++ ")"

instance Show Statement where
    show state = showStatement state ""
                 
instance Show Function where
    show (Func t ident var state) =
        foldr (++) "" ["(",show t," ",show ident," (",show var,")",showBody state,")"]

showBody st = showStatement st "    "
