module Main where
import Control.Monad
import System.Environment
import Control.Monad.Error
import Control.Applicative hiding ((<|>))
import Data.IORef
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
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
spaces1 :: Parser ()
spaces1 = skipMany1 space

data CVal = Atom String
	  | Number Integer
	  | Variation String
	  | NullExp ()
	  | Add CVal CVal
	  | Sub CVal CVal
	  | Mul CVal CVal
	  | Div CVal CVal
	  | Mod CVal CVal

showVal :: CVal -> String
showVal (Atom name) = name
showVal (Variation name) = name
showVal (NullExp _) = ""
showVal (Number n) = show n
showVal (Add n1 n2) = "(+ " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
showVal (Sub n1 n2) = "(- " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
showVal (Mul n1 n2) = "(* " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
showVal (Div n1 n2) = "(/ " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
showVal (Mod n1 n2) = "(% " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"

instance Show CVal where show = showVal

parseExpr    :: Parser CVal
parseExpr    = buildExpressionParser table parseFactor

table   = [[op "*" Mul AssocLeft, op "/" Div AssocLeft, op "%" Mod AssocLeft]
          ,[op "+" Add AssocLeft, op "-" Sub AssocLeft]
          ]
        where
          op s f assoc
             = Infix (do{ string s; return f}) assoc

parseFactor :: Parser CVal
parseFactor = do
    whiteSpace
    f <- parens parseExpr
         <|> parseVar
    whiteSpace
    return f

parseStatementList :: Parser [CVal]
parseStatementList = do
    s <- parseStatement
    ((:) s <$> (whiteSpace *> parseStatementList)) <|> pure (s : [])
    
parseStatement :: Parser CVal
parseStatement =
    do whiteSpace >> semi
       return $ NullExp ()
    <|> do exp <- parseExpr
           whiteSpace >> semi
           return exp

parseProgram :: Parser CVal
parseProgram = do
    whiteSpace
    exp <- parseStatement
    whiteSpace
    return exp

parseVar :: Parser CVal
parseVar = do 
    whiteSpace
    n <- parseNumber
         <|> parseDeclarator
    whiteSpace
    return n

parseParameterDeclaration :: Parser CVal
parseParameterDeclaration = do
    whiteSpace
    char 'i' >> char 'n' >> char 't' >> spaces1
    p <- parseDeclarator
    return p

parseDeclarationList :: Parser [CVal]
parseDeclarationList = do
    d <- parseDeclaration
    ((++) d <$> (whiteSpace *> parseDeclarationList)) <|> pure d
    
parseDeclaration :: Parser [CVal]
parseDeclaration = do
    char 'i' >> char 'n' >> char 't'
    spaces1
    pl <- parseDeclaratorList
    whiteSpace >> semi
    return pl
    
parseDeclarator :: Parser CVal
parseDeclarator = do
    v <- identifier
    return $ Variation v

parseDeclaratorList :: Parser [CVal]
parseDeclaratorList = do
    p <- parseDeclarator
    ((:) p <$> (whiteSpace *> comma *> whiteSpace *> parseDeclaratorList)) <|> pure (p : [])

parseNumber :: Parser CVal
parseNumber = liftM (Number . read) $ many1 digit

main :: IO ()
main = do
   input <- getLine
   print $ case parse parseStatementList "TinyC" input of
      Left err -> show err
      Right val -> show val
