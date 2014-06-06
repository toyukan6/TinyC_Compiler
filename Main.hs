module Main where

import Control.Monad
import System.Environment
import Control.Monad.Error
import Control.Applicative hiding ((<|>), many)
import Data.IORef
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language
import GHC.IO.Handle
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

