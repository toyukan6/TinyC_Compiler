module Syntax.Semantic where

import qualified Data.Map as Map
    
import CompileError
import Syntax.AST
import Syntax.Type

--à”ñ°âêÕå„ÇÃProgram
data SVal = SFunc FuncObj
          | STmpFunc TmpFuncObj
          | SDecl VarObj

instance Show SVal where
    show (SFunc f) = show f
    show (STmpFunc tmp) = show tmp
    show (SDecl v) = show v

data SIdentifier = SIdentifier {
      name :: String,
      address :: Integer }

instance Show SIdentifier where
    show (SIdentifier n l) = "(" ++ n ++ ":" ++ show l ++ ")"

data SCVal = SNumber Integer
           | SIdent SIdentifier
           | SMinus SCVal
           | SCValList SCVal SCVal
           | SCalFunc SIdentifier [SCVal]
           | SAssign SIdentifier SCVal
           | SAdd SCVal SCVal
	   | SSub SCVal SCVal
	   | SMul SCVal SCVal
	   | SDiv SCVal SCVal
	   | SMod SCVal SCVal
	   | SMore SCVal SCVal
	   | SLess SCVal SCVal
	   | SMoreE SCVal SCVal
	   | SLessE SCVal SCVal
	   | SEqual SCVal SCVal
	   | SNEqual SCVal SCVal
	   | SL_AND SCVal SCVal
	   | SL_OR SCVal SCVal

instance Show SCVal where
    show (SNumber n) = show n
    show (SIdent i) = show i
    show (SMinus m) = "-" ++ show m
    show (SCValList l1 l2) = show l1 ++ show l2
    show (SCalFunc ident var) = "(" ++ show ident ++ " (" ++ unwordsList var ++ "))"
    show (SAssign n1 n2) = "(= " ++ show n1 ++ " " ++ show n2 ++ ")"
    show (SAdd n1 n2) = showSExpr "+" n1 n2
    show (SSub n1 n2) = showSExpr "-" n1 n2
    show (SMul n1 n2) = showSExpr "*" n1 n2
    show (SDiv n1 n2) = showSExpr "/" n1 n2
    show (SMod n1 n2) = showSExpr "%" n1 n2
    show (SMore n1 n2) = showSExpr "<" n1 n2
    show (SLess n1 n2) = showSExpr ">" n1 n2
    show (SMoreE n1 n2) = showSExpr "<=" n1 n2
    show (SLessE n1 n2) = showSExpr ">=" n1 n2
    show (SEqual n1 n2) = showSExpr "==" n1 n2
    show (SNEqual n1 n2) = showSExpr "!=" n1 n2
    show (SL_AND n1 n2) = showSExpr "and" n1 n2
    show (SL_OR n1 n2) = showSExpr "or" n1 n2

showSExpr :: String -> SCVal -> SCVal -> String
showSExpr s c1 c2 = "(" ++ s ++ " " ++ show c1 ++ " " ++ show c2 ++ ")"

data SStatement = SNullExp
                | SExpression SCVal
                | SIf { tag :: Integer,
                        condition :: SCVal,
                        state1 :: SStatement,
                        elsestate ::  SStatement }
                | SWhile { tag :: Integer,
                           condition :: SCVal,
                           state :: SStatement }
                | SReturn { tag :: Integer,
                            exp :: (Maybe SCVal) }
                | SDeclaration [VarObj]
                | SCompoundStatement [SStatement]

instance Show SStatement where
    show SNullExp = ""
    show (SExpression val) = show val
    show (SIf t c s e) = "(if : " ++ show t ++ show c ++ show s ++ show e ++ ")"
    show (SWhile t c s) = "(while : " ++ show t ++ show c ++ show s ++ ")"
    show (SReturn t Nothing) = "(return : " ++ show t ++ ")"
    show (SReturn t (Just e)) = "(return : " ++ show t ++ show e ++ ")"
    show (SDeclaration var) = unwordsList var
    show (SCompoundStatement state) = "(" ++ unwordsList state ++ ")"

data TmpFuncObj = TmpFuncObj {
      tmpFname :: String,
      tmpParams :: [String],
      tmpParamTypes :: [SType],
      tmpReturnType :: SType }

instance Show TmpFuncObj where
    show (TmpFuncObj n p t r) =
        "(" ++ show r ++ " " ++ n ++ "(" ++  unwords zw ++ "))"
        where zw = zipWith (++) (map show t) (map ((++) ":" . show) p)

data FuncObj = FuncObj {
    fname :: SIdentifier,
    params :: [VarObj],
    returnType :: SType,
    body :: SStatement }

instance Show FuncObj where
    show (FuncObj n p r b) = 
        "(" ++ show r ++ " " ++ show n ++ "(" ++  unwordsList p ++ ")" ++ show b ++ ")"
    
data VarObj = VarObj {
    vname :: String,
    vType :: SType,
    vAddress :: Integer,
    vLevel :: Integer }

instance Show VarObj where
    show (VarObj n t a l) =
        "(" ++ show t ++ " " ++ n ++ ":" ++ show a ++ ":" ++ show l ++ ")"

data SType = SInt
           | SVoid
	   | SUndefined
	   deriving(Show)

type SValTable = Map.Map Integer SVal                   
type GlobalSValTable = Map.Map String SVal

sizeOf :: SVal -> Integer
sizeOf (SDecl var) = sizeOfType (vType var)
sizeOf _ = -1                     
    
emptyTable :: SValTable
emptyTable = Map.empty              
    
addSVal :: SValTable -> SVal -> SValTable
addSVal table val = Map.insert (toInteger $ Map.size table) val table
    
sizeOfType :: SType -> Integer
sizeOfType SInt = 4
sizeOfTYpe SVoid = error "why do you need size of void?"
--sizeOfType _ = -1

convT :: Type -> SType
convT CInt = SInt
convT CVoid = SVoid
	  
