module Syntax.Semantic where

import qualified Data.Map as Map
    
import CompileError
import Syntax.AST
import Syntax.Type

--ˆÓ–¡‰ðÍŒã‚ÌProgram
data SVal = SFunc FuncObj
          | STmpFunc TmpFuncObj
          | SDecl VarObj
	  deriving (Show)

data SIdentifier = SIdentifier {
      name :: String,
      level :: Integer }
      deriving(Show)

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
           deriving(Show)

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
                deriving (Show)

data TmpFuncObj = TmpFuncObj {
      tmpFname :: String,
      tmpParams :: [String],
      tmpParamTypes :: [SType],
      tmpReturnType :: SType }
      deriving (Show)

data FuncObj = FuncObj {
    fname :: SIdentifier,
    params :: [VarObj],
    returnType :: SType,
    body :: SStatement }
    deriving (Show)
    
data VarObj = VarObj {
    vname :: String,
    vType :: SType,
    vAddress :: Integer,
    vLevel :: Integer }
    deriving (Show)

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
	  
