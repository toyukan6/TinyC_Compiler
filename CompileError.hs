module CompileError where

import qualified Text.Parsec.Error as PE

import Syntax.AST
import Syntax.Type

data Warning = ParamShadow String
             | CallUndefineFunction String
             deriving (Eq)

undefinedFunctions :: [Warning] -> [String]
undefinedFunctions [] = []
undefinedFunctions (CallUndefineFunction cuf : ws) = cuf : (undefinedFunctions ws)
undefinedFunctions (ParamShadow _ : ws) = undefinedFunctions ws

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

data CompileError = PError PE.ParseError
                  | SError SemanticError
                  | SWarning Warning

instance Show CompileError where
    show (PError err) = "Parse Error : " ++ show err
    show (SError err) = show err
    show (SWarning war) = show war

logToError :: [CompileLog] -> [CompileError]
logToError [] = []
logToError (Err e:ls) = (:) (SError e) . logToError $ ls
logToError (War w:ls) = (:) (SWarning w) . logToError $ ls

isErr :: CompileLog -> Bool
isErr (Err _) = True
isErr (War _) = False

errs :: [CompileLog] -> [SemanticError]
errs [] = []
errs (Err e:ls) = (:) e . errs $ ls
errs (War w:ls) = errs ls

isWar :: CompileLog -> Bool
isWar (War _) = True
isWar (Err _) = False

wars :: [CompileLog] -> [Warning]
wars [] = []
wars (Err r: ls) = wars ls
wars (War w:ls) = (:) w . wars $ ls

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