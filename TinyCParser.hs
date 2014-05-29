module Main where
import Control.Monad
import System.Environment
import Control.Monad.Error
import Data.IORef
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language
import GHC.IO.Handle
import System.IO

tinyCStyle = emptyDef {
	   commentStart   = "/*"
           , commentEnd     = "*/"
           , commentLine    = "//"
           , nestedComments = True
           , identStart     = letter <|> char '_'
           , identLetter    = alphaNum <|> oneOf "_"
           , opStart        = opLetter emptyDef
           , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
           , reservedOpNames= []
           , reservedNames  = ["if", "else", "while", "return", "int", "void"]
           , caseSensitive  = True
           }

lexer :: Token.TokenParser ()
lexer  = Token.makeTokenParser tinyCStyle

whiteSpace :: Parser () 
whiteSpace = Token.whiteSpace lexer
lexeme     :: Parser a -> Parser a
lexeme     = Token.lexeme lexer  
symbol     :: String -> Parser String
symbol     = Token.symbol lexer
natural    :: Parser Integer
natural    = Token.natural lexer
identifier :: Parser String
identifier = Token.identifier lexer
reserved   :: String -> Parser ()
reserved   = Token.reserved lexer
operator   :: Parser String
operator   = Token.operator lexer
reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer
parens     :: Parser a -> Parser a
parens	      = Token.parens lexer
braces     :: Parser a -> Parser a
braces	      = Token.braces lexer
squares    :: Parser a -> Parser a
squares    = Token.squares lexer
semi       :: Parser String
semi       = Token.semi lexer
comma      :: Parser String
comma      = Token.comma lexer

data CVal = Atom String
          | Integer Integer
	  | Number Integer
	  | Variation String
	  | Expression {left :: CVal, op :: Char, right :: CVal}

--idnetifier :: Parser String
--identifier = do
--    c <- char '_' <|> letter
--    cs <- many (try (alphaNum) <|> try(OneOf "_$"))
--    return (c : cs)
	  
parseAdd :: Parser CVal
parseAdd = do
   x <- parseExpression
   whiteSpace >> char '+' >> whiteSpace
   y <- parseMul
   return $ Expression x '+' y

parseAddOrSub :: Parser CVal
parseAddOrSub = do
    whiteSpace
    x <- try parseMul
         <|> try parseAdd
    return x

parseMul :: Parser CVal
parseMul = do
   x <- parseNumber
   whiteSpace >> char '*' >> whiteSpace
   y <- try parseMul
        <|> parseNumber
   return $ Expression x '*' y
    
parseExpression :: Parser CVal
parseExpression = do
    whiteSpace
    exp <- parseAddOrSub
    whiteSpace >> semi
    return exp

parseProgram :: Parser CVal
parseProgram = do
    whiteSpace
    exp <- parseExpression
    whiteSpace
    return exp

parseNumber :: Parser CVal
parseNumber = liftM (Number . read) $ many1 digit

main :: IO ()
main = do
   input <- getLine
   print $ case parse parseProgram "TinyC" input of
      Left err -> "Error"
      Right val -> "Succeed"
