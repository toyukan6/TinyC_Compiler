module Main where

import Text.ParserCombinators.Parsec
import System.Environment
import System.FilePath (dropExtension)

import CodeGenerator.TinyCGenerator
import SemanticChecker.TinyCChecker
import CompileError
import Parser.TinyCParser
import Syntax.AST
import Syntax.Type
import Syntax.Semantic
import Syntax.Generator

generate :: ([CompileLog], GlobalSValTable) -> Either [CompileError] ([CompileLog], [Code])
generate (log, table) =
    Right (log, generateCode table)

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
  file <- readFile $ head input
  let code = parser file >>= checker >>= generate
  case code of
    Left err -> putStr . unlines . map show $ err
    Right val -> do
              let log = fst val
              putStr . unlines . map show $ log
              writeFile (dropExtension (head input) ++ ".asm") . unlines . map show . snd $ val


