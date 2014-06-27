module CompileError where

import qualified Text.Parsec.Error as PE

import Syntax.AST
import Syntax.Type

data Warning = ParamShadow String
             | CallUndefineFunction String
             deriving (Eq)

data SemanticError = UndefinedVariable String
     		   | ReDeclaration String
                   | ReDeclarationDifferentType String
                   | TypeError String
                   | FunctionCallWithVariable String
                   | VariableWithFunctionCall String
                   | InvalidNumOfParameter String Integer Integer
		   deriving (Eq)
		   
data CompileLog = Err SemanticError
                | War Warning
                deriving (Eq)

isErr :: CompileLog -> Bool
isErr (Err _) = True
isErr (War _) = False

isWar :: CompileLog -> Bool
isWar (War _) = True
isWar (Err _) = False

instance Show Warning where
    show (ParamShadow ident) = "declaration of " ++ ident ++ " shadows a parameter"
    show (CallUndefineFunction s) = "call undefined function " ++ s

instance Show SemanticError where
    show (UndefinedVariable s) = "undefined variable " ++ s
    show (ReDeclaration s) = "redeclaration of " ++ s
    show (ReDeclarationDifferentType s) = "redeclaration of different type : " ++ s
    show (TypeError s) = "TypeError : " ++ s
    show (FunctionCallWithVariable s) = s ++ " is not a function"
    show (VariableWithFunctionCall s) = s ++ " is a function"
    show (InvalidNumOfParameter s expect given) = s ++ " is expected " ++ show expect ++ " parameters, but " ++ show given ++ " parameters given"
    
instance Show CompileLog where
    show (Err sem) = show sem
    show (War war) = show war

addRedeclarationError :: [CompileLog] -> String -> [CompileLog]
addRedeclarationError log x = log ++ [Err $ ReDeclaration x]