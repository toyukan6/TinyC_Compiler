module Main where

import Text.ParserCombinators.Parsec
import System.Environment

import SemanticChecker.TinyCChecker
import Parser.TinyCParser
import Syntax.AST
import Syntax.Type

checker :: String -> IO ()
checker input = do
  p <- parser input
  putStrLn . show . createTable $ p

parser :: String -> IO [Program]
parser input = do
  file <- readFile input
  case parse parseProgram "TinyC" file of
     Left _ -> return []
     Right val -> return val

main :: IO ()
main = do
   input <- getArgs
   file <- readFile (head input)
   putStrLn $ case parse parseProgram "TinyC" file of
      Left err -> show err
      Right val -> init . unlines . map show $ val

