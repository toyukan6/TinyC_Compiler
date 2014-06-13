module Main where

import Text.ParserCombinators.Parsec
import System.Environment

import SemanticChecker.TinyCChecker
import Parser.TinyCParser
import Syntax.AST
import Syntax.Type

main :: IO ()
main = do
   input <- getArgs
   file <- readFile (head input)
   putStrLn $ case parse parseProgram "TinyC" file of
      Left err -> show err
      Right val -> unwords . map ((++) "\n" . show) $ val

