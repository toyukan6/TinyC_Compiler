module Main where

import Text.ParserCombinators.Parsec
import System.Environment

import SemanticChecker.TinyCChecker
import CompileError
import Parser.TinyCParser
import Syntax.AST
import Syntax.Type
import Syntax.Semantic

checker :: [Program] -> Either [CompileError] ([CompileLog], GlobalSValTable)
checker pro =
    let sc@(log, table) = createTable pro
    in if null log || all isWar log
       then Right sc
       else Left . logToError $ log

parser :: String -> Either [CompileError] [Program]
parser input =
    case parse parseProgram "TinyC" input of
      Left err -> Left [PError err]
      Right val -> Right val

main :: IO ()
main = do
  input <- getArgs
  file <- readFile (head input)
  let code = parser file >>= checker
  case code of
    Left err -> putStr . unlines . map show $ err
    Right val -> putStrLn . show $ val


