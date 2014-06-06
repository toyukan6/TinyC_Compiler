module Main where

import Text.ParserCombinators.Parsec
import System.Environment

import Parser.TinyCParser
import Syntax.AST
import Syntax.Type

main :: IO ()
main = do
   input <- getArgs
   file <- readFile (head input)
   print $ case parse parseProgram "TinyC" file of
      Left err -> show err
      Right val -> show val

