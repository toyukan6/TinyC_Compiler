module Syntax.Generator where

import Syntax.Semantic

data BinaryOperation = GAdd String String
                     | GSub String String
                     | GMul String String
                     | GCmp String String
                     | GMov String String
                     | GMovzx String String
                     | GCommon String Integer

binaryOperation :: String -> String -> String -> String
binaryOperation op e1 e2 = op ++ " " ++ e1 ++ ", " ++ e2

instance Show BinaryOperation where
    show (GAdd e1 e2) = binaryOperation "add" e1 e2
    show (GSub e1 e2) = binaryOperation "sub" e1 e2
    show (GMul e1 e2) = binaryOperation "imul" e1 e2
    show (GCmp e1 e2) = binaryOperation "cmp" e1 e2
    show (GMov e1 e2) = binaryOperation "mov" e1 e2
    show (GMovzx e1 e2) = binaryOperation "movzx" e1 e2
    show (GCommon l n) = "COMMON" ++ " " ++ l ++ " " ++ show n

data Label = Label String

instance Show Label where
    show (Label l) = l ++ ":"

data MonadicOperation = GDiv String
                      | GMod String
                      | GPush String
                      | GPop String
                      | GGlobal String
                      | GExtern String
                      | GCall String
                      | GJump Label
                      | GJe Label
                      | GNot String

monadicOperation :: String -> String -> String
monadicOperation op e = op ++ " " ++ e

instance Show MonadicOperation where
    show (GDiv e) = monadicOperation "idiv" e
    show (GMod e) = monadicOperation "idiv" e
    show (GPush e) = monadicOperation "push" e
    show (GPop e) = monadicOperation "pop" e
    show (GGlobal e) = monadicOperation "GLOBAL" e
    show (GExtern e) = monadicOperation "EXTERN" e
    show (GCall e) = monadicOperation "call" e
    show (GJump l) = monadicOperation "jmp" . init . show $ l
    show (GJe l) = monadicOperation "je" . init . show $ l
    show (GNot n) = monadicOperation "not" n

data ZeroOperation = GRet
                   | GSetg
                   | GSetge
                   | GSetl
                   | GSetle
                   | GSete
                   | GCdq

instance Show ZeroOperation where
    show GRet = "ret"
    show GSetg = "setg al"
    show GSetge = "setge al"
    show GSetl = "setl al"
    show GSetle = "setle al"
    show GSete = "sete al"
    show GCdq = "cdq"

data Code = BO BinaryOperation
          | MO MonadicOperation
          | ZO ZeroOperation
          | L Label

instance Show Code where
    show (BO b) = "\t" ++ show b
    show (MO m) = "\t" ++ show m
    show (ZO z) = "\t" ++ show z
    show (L l) = show l