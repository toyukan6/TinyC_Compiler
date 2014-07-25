module Syntax.Semantic where

import Data.Maybe
import qualified Data.Map as Map
    
import CompileError
import Syntax.AST
import Syntax.Type

--ˆÓ–¡‰ðÍŒã‚ÌProgram
data SVal = SFunc FuncObj
          | STmpFunc TmpFuncObj
          | SDecl VarObj

instance Show SVal where
    show (SFunc f) = show f
    show (STmpFunc tmp) = show tmp
    show (SDecl v) = show v

isSFunc (SFunc f) = True
isSFunc _ = False

isSTmpFunc (STmpFunc f) = True
isSTmpFunc _ = False

isSDecl (SDecl d) = True
isSDecl _ = False

data SIdentifier = SIdentifier {
      name :: String,
      address :: Integer }

instance Show SIdentifier where
    show (SIdentifier n l) = "(" ++ n ++ ":" ++ show l ++ ")"

data SCVal = SNumber Integer
           | SIdent SIdentifier
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
	   | SL_AND String SCVal SCVal
	   | SL_OR String SCVal SCVal
           | TmpVar VarObj

constructorToOp :: SCVal -> (Integer -> Integer -> Integer)
constructorToOp (SAdd _ _) = (+)
constructorToOp (SSub _ _) = (-)
constructorToOp (SMul _ _) = (*)
constructorToOp (SDiv _ _) = div
constructorToOp (SMod _ _) = mod

tmpVars :: SCVal -> [VarObj]
tmpVars (TmpVar v) = [v]
tmpVars (SCValList l ls) = (tmpVars l) ++ (tmpVars ls)
tmpVars (SCalFunc i vl) = foldr (++) [] . map tmpVars $ vl
tmpVars (SAssign i v) = tmpVars v
tmpVars (SAdd var1 var2) = (tmpVars var1) ++ (tmpVars var2)
tmpVars (SSub var1 var2) = (tmpVars var1) ++ (tmpVars var2)
tmpVars (SMul var1 var2) = (tmpVars var1) ++ (tmpVars var2)
tmpVars (SDiv var1 var2) = (tmpVars var1) ++ (tmpVars var2)
tmpVars (SMod var1 var2) = (tmpVars var1) ++ (tmpVars var2)
tmpVars (SMore var1 var2) = (tmpVars var1) ++ (tmpVars var2)
tmpVars (SLess var1 var2) = (tmpVars var1) ++ (tmpVars var2)
tmpVars (SMoreE var1 var2) = (tmpVars var1) ++ (tmpVars var2)
tmpVars (SLessE var1 var2) = (tmpVars var1) ++ (tmpVars var2)
tmpVars (SEqual var1 var2) = (tmpVars var1) ++ (tmpVars var2)
tmpVars (SNEqual var1 var2) = (tmpVars var1) ++ (tmpVars var2)
tmpVars (SL_AND _ var1 var2) = (tmpVars var1) ++ (tmpVars var2)
tmpVars (SL_OR _ var1 var2) = (tmpVars var1) ++ (tmpVars var2)
tmpVars _ = []

instance Show SCVal where
    show (SNumber n) = show n
    show (SIdent i) = show i
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
    show (SL_AND t n1 n2) = showSExpr ("and" ++ t) n1 n2
    show (SL_OR t n1 n2) = showSExpr ("or" ++ t) n1 n2
    show (TmpVar v) = show v

showSExpr :: String -> SCVal -> SCVal -> String
showSExpr s c1 c2 = "(" ++ s ++ " " ++ show c1 ++ " " ++ show c2 ++ ")"

data SStatement = SNullExp
                | SExpression SCVal
                | SIf { itag :: String,
                        condition :: SCVal,
                        state1 :: SStatement,
                        elsestate ::  SStatement }
                | SWhile { wtag :: String,
                           condition :: SCVal,
                           state :: SStatement }
                | SFor { fortag :: String,
                         initialize :: (Maybe SCVal),
                         fcondition :: (Maybe SCVal),
                         fupdate :: (Maybe SCVal),
                         fstate :: SStatement }
                | SReturn { rfuncName :: String,
                            rExp :: Maybe SCVal }
                | SDeclaration [VarObj]
                | SCompoundStatement [SStatement]

declarations :: [SStatement] -> [VarObj]
declarations [] = []
declarations (SExpression exp : ss) = (tmpVars exp) ++ (declarations ss)
declarations (SDeclaration sd : ss) = (++) sd . declarations $ ss
declarations (SCompoundStatement scs : ss) = (declarations scs) ++ (declarations ss)
declarations (SIf _ cond s1 s2 : ss) =
    (tmpVars cond) ++ (declarations [s1]) ++ (declarations [s2]) ++ (declarations ss)
declarations (SWhile _ cond s : ss) =
    (tmpVars cond) ++ (declarations [s]) ++ (declarations ss)
declarations (SFor _ init cond up s : ss) =
    (foldr (++) [] . map tmpVars . catMaybes $ [init, cond, up])
                       ++ (declarations [s]) ++ (declarations ss)
declarations (SReturn _ Nothing : ss) = declarations ss
declarations (SReturn _ (Just e) : ss) =
    (tmpVars e) ++ (declarations ss)
declarations (_ : ss) = declarations ss

instance Show SStatement where
    show SNullExp = ""
    show (SExpression val) = show val
    show (SIf t c s e) = "(if : " ++ t ++ " " ++ show c ++ show s ++ show e ++ ")"
    show (SWhile t c s) = "(while : " ++ t ++ " " ++ show c ++ show s ++ ")"
    show (SFor t i c u s) =
        "(for : " ++ t ++ " " ++ unwordsList [i, c, u] ++ " " ++ show s ++ ")"
    show (SReturn _ Nothing) = "(return)"
    show (SReturn _ (Just expr)) = "(return " ++ show expr ++ ")"
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

data VarObj = VarObj { vname :: String,
                       vType :: SType,
                       vAddress :: Integer,
                       vLevel :: Integer }
            | TmpVarObj { vname :: String,
                          vType :: SType,
                          vAddress :: Integer,
                          vLevel :: Integer,
                          tmpvExp :: SCVal }

instance Show VarObj where
    show (VarObj n t a l) =
        "(" ++ show t ++ " " ++ n ++ ":" ++ show a ++ ":" ++ show l ++ ")"
    show (TmpVarObj n t a l e) = "(" ++ n ++ ":" ++ show e ++ ":" ++ show a ++ ")"

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
	  
