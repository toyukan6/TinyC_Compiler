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

test :: (Show a) => Parser a -> IO ()
test parser = do
    input <- getLine
    print $ case parse parser "TinyC" input of
        Left err -> show err
	Right val -> show val

test2 :: Parser [CVal] -> IO ()
test2 parser = do
    input <- getLine
    print $ case parse parser "TinyC" input of
        Left err -> show err
	Right val -> unwords . map show $ val
	
main :: IO ()
main = do
   input <- getArgs
   print $ case parse parseStatement "TinyC" input of
      Left err -> show err
      Right val -> show val
