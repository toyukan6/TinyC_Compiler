module CompileError where

import qualified Text.Parsec.Error as PE

import Syntax.AST
import Syntax.Type

data Warning = ParamShadow String
             | CallUndefineFunction String

data SemanticError = UndefinedVariable String
     		   | ReDeclaration String
                   | ReDeclarationDifferentType String
                   | TypeError String
                   | FunctionCallWithVariable String
                   | InvalidNumOfParameter Integer Integer
		   deriving (Eq)
		   
data CompileError = ParseError PE.ParseError
                  | SemantError [CompileLog]

data CompileLog = Err SemanticError
                | War Warning

instance Show Warning where
    show (ParamShadow ident) = "declaration of " ++ ident ++ " shadows a parameter"
    show (CallUndefineFunction s) = "call undefined function " ++ s

instance Show SemanticError where
    show (UndefinedVariable s) = "undefined variable " ++ show s
    
instance Show CompileError where
    show (ParseError err) = "Parse Error : " ++ show err
    show (SemantError err) = show err

instance Show CompileLog where
    show (Err sem) = show sem
    show (War war) = show war