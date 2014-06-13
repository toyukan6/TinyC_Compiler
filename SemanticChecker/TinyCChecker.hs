module SemanticChecker.TinyCChecker where

import qualified Data.Map as Map

import CompileError
import Syntax.AST
import Syntax.Type
import Syntax.Semantic
{-
semanticCheck :: Program -> SVal
semanticCheck (PFunc func)

functionCheck :: Function -> SVal
functionCheck func =
-}
makeSFunc :: Function -> SVal
makeSFunc (Func tp (Identifier name) (ParameterDecl ps) _) =
    SFunc FuncObj { fname = name,
                    params = snd $ mp ps,
                    paramTypes = fst $ mp ps,
                    returnType = convT tp }
    where mp [] = ([],[])
          mp ((Variation t (Identifier nm)):xs) = (convT t : fst (mp xs), nm : snd (mp xs))        

makeGlobalSDecl :: Variation -> SVal
makeGlobalSDecl (Variation tp (Identifier name)) =
    SDecl VarObj { vname = name,
                   vType = convT tp,
                   vAddress = 0,
                   vLevel = 0 }           

type Stack = [(Integer, String, Integer)]
    
--createTable program = collectGlobalSVals Map.empty [] program
{-
collectGlobalSVals :: GlobalSValTable -> [CompileLog]
		      		      -> [Program]
				      -> (GlobalSValTable, [CompileLog])
collectGlobalSVals stable compileLog [] = (stable, compileLog)
-}
--collectGlobalSVals stable compileLog (PFunc func : xs) =
--    let sym@(
{-
makeFuncSVal :: Function -> SVal
makeFuncSVal (Func tp (Identifier name) (ParameterDecl param) _)  =
    SFunc FuncObj { fname = name,
    	            params = snd $ mp param,
		    paramTypes = fst $ mp param,
		    returnType = convT tp }
    where mp [] = ([], [])
          mp ((t, Identifier nm) : xs) = (convT t : fst (mp xs), nm : snd (mp xs))
-}