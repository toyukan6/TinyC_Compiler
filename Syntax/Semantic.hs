module Syntax.Semantic where

import qualified Data.Map as Map
    
import CompileError
import Syntax.AST
import Syntax.Type

--意味解析後のProgram
data SVal = SFunc FuncObj
          | SDecl VarObj
	  deriving (Show)

type GlobalSValTable = Map.Map String SVal
                   
data FuncObj = FuncObj {
    fname :: String,
    params :: [String],
    paramTypes :: [SType],
    returnType :: SType }
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

convT :: Type -> SType
convT CInt = SInt
convT CVoid = SVoid
	  
