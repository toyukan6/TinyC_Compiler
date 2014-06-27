module Main where

import Text.ParserCombinators.Parsec
import System.Environment

import SemanticChecker.TinyCChecker
import Parser.TinyCParser
import Syntax.AST
import Syntax.Type

test :: (Show a) => Parser a -> String -> IO ()
test parser input = 
   putStrLn $ case parse parser "TinyC" input of
      Left err -> show err
      Right val -> show val

main :: IO ()
main = do
   input <- getArgs
   file <- readFile (head input)
   putStrLn $ case parse parseProgram "TinyC" file of
      Left err -> show err
      Right val -> init . unlines . map show $ val

