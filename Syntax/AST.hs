module Syntax.AST where

import qualified Data.Map as Map
import Syntax.Type

--Ž¯•ÊŽq‚ÌŒ^
data Identifier = Identifier String

instance Show Identifier where
    show (Identifier s) = s

--ƒp[ƒXŒ‹‰Ê‚ÌŒ^
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

--•Ï”‚ÌŒ^
data Variation = Variation Type Identifier

instance Show Variation where
    show (Variation t ident) = "(" ++ show t ++ " " ++ show ident ++ ")"

--statement‚Ìƒp[ƒXŒ‹‰Ê	  
data Statement = NullExp
               | Expression CVal
	       | If CVal Statement Statement
	       | While CVal Statement
	       | Return (Maybe CVal)
	       | Declaration [Variation]
               | CompoundStatement [Statement]

--ƒpƒ‰ƒ[ƒ^éŒ¾‚ÌŒ^
data ParamDecl = ParameterDecl [Variation]
instance Show ParamDecl where
    show (ParameterDecl var) = unwordsList var

--ŠÖ”‚ÌŒ^    
data Function = Func Type Identifier ParamDecl Statement

data FuncObj = FuncObj {
    fname :: String,
    params :: [String],
    paramTypes :: [SType],
    returnType :: SType }
    deriving (Show)
    
data VarObj = VarObj {
    vname :: String,
    vType :: SType }
    deriving (Show)

data SVal = SFunc FuncObj
          | SVariation VarObj
	  deriving (Show)

data SType = SInt
           | SVoid
	   | SUndefined
	   deriving(Show)

convT :: Type -> SType
convT CInt = SInt
convT CVoid = SVoid
	  
type GlobalSValTable = Map.Map String SVal

--ƒvƒƒOƒ‰ƒ€‚ÌŒ^
data Program = PDecl Statement
             | PFunc Function

instance Show Program where
    show (PDecl state) = show state
    show (PFunc func) = show func

unwordsList :: (Show a) => [a] -> String
unwordsList l = unwords . map show $ l

indent :: (Show a) => [a] -> String
indent l = unwords . map ((++) "    " . show) $ l
    
showExpr :: String -> CVal -> CVal -> String
showExpr s c1 c2 = "(" ++ s ++ " " ++ show c1 ++ " " ++ show c2 ++ ")"

showVal :: CVal -> String
showVal (Atom name) = name
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

showStatement :: Statement -> String
showStatement NullExp = "()"
showStatement (Expression val) = show val
showStatement (If cond state1 state2) = "(if " ++ show cond ++ " " ++ show state1 ++ " " ++ show state2 ++ ")"
showStatement (While cond state) = "(while " ++ show cond ++ " " ++ show state ++ ")"
showStatement (Return (Just state)) = "(return " ++ show state ++ ")"
showStatement (Return Nothing) = "(return)"
showStatement (Declaration var) = unwordsList var
showStatement (CompoundStatement state) = "(" ++ unwordsList state ++ ")"

instance Show Statement where show = showStatement

instance Show Function where
    show (Func t ident var state) = "(" ++ show t ++ " " ++ show ident ++ " (" ++ show var ++ ") " ++ show state ++ ")"
