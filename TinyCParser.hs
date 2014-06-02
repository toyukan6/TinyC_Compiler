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
	  | Minus CVal
	  | Variation String
	  | NullExp ()
	  | List [CVal]
	  | CalFunc String [CVal]
	  | Expression String CVal CVal
	  | CompoundStatement [CVal] [CVal]

showVal :: CVal -> String
showVal (Atom name) = name
showVal (Variation name) = name
showVal (NullExp _) = ""
showVal (Number n) = show n
showVal (Minus n) = '-' : show n
showVal (List l) = unwords . map show $ l
showVal (CalFunc ident var) = "(" ++ ident ++ " (" ++ unwords (map show var) ++ "))"
showVal (Expression op n1 n2) = "(" ++ op ++ " " ++ showVal n1 ++ " " ++ showVal n2 ++ ")"
showVal (CompoundStatement var exp) = "((" ++ unwords (map show var) ++ ")(" ++ unwords (map show exp) ++ "))"

instance Show CVal where show = showVal

parseLogicalORExpr    :: Parser CVal
parseLogicalORExpr    = buildExpressionParser table parseFactor

table   = [[op "*" (Expression "*") AssocLeft, op "/" (Expression "/") AssocLeft, op "%" (Expression "%") AssocLeft]
          ,[op "+" (Expression "+") AssocLeft, op "-" (Expression "-") AssocLeft]
          ,[op "<" (Expression "<") AssocLeft, op ">" (Expression ">") AssocLeft, op "<=" (Expression "<=") AssocLeft, op ">=" (Expression ">=") AssocLeft]
          ,[op "==" (Expression "==") AssocLeft, op "!=" (Expression "!=") AssocLeft]
          ,[op "&&" (Expression "&&") AssocLeft]
	  ,[op "||" (Expression "||") AssocLeft]
          ]
        where
          op s f assoc
             = Infix (do{ string s; return f}) assoc

parseExpression :: Parser [CVal]
parseExpression = do
    assign <- parseAssignExpr
    ((:) assign <$> (whiteSpace *> comma *> whiteSpace *> parseExpression)) <|> pure (assign : [])

parseAssignExpr :: Parser CVal
parseAssignExpr = try parseLogicalORExpr
                  <|> parseSubstitution

parseSubstitution :: Parser CVal
parseSubstitution = do
    whiteSpace
    i <- parseDeclarator
    whiteSpace >> char '=' >> whiteSpace
    r <- parseAssignExpr
    return $ Expression "=" i r

parseUnaryExpr :: Parser CVal
parseUnaryExpr = try parsePostfixExpr
    <|> do whiteSpace >> char '-'
           ue <- parseUnaryExpr
	   whiteSpace
	   return $ Minus ue

parsePostfixExpr :: Parser CVal
parsePostfixExpr = try parseFactor
                   <|> parseCalFunc

parseCalFunc :: Parser CVal
parseCalFunc = try (do
    i <- identifier
    whiteSpace >> char '('
    ae <- parseArgumentExpressionList
    whiteSpace >> char ')'
    return $ CalFunc i ae)
    <|> do i <- identifier
           whiteSpace >> char '(' >> whiteSpace >> char ')' >> whiteSpace
	   return $ CalFunc i []

parseArgumentExpressionList :: Parser [CVal]
parseArgumentExpressionList = do
    whiteSpace
    ae <- parseAssignExpr
    ((:) ae <$> (whiteSpace *> comma *> whiteSpace *> parseArgumentExpressionList)) <|> pure (ae : [])

parseFactor :: Parser CVal
parseFactor = do
    whiteSpace
    f <- parseVar
    whiteSpace
    return f
    <|> do whiteSpace
           f <- try (parens parseExpression)
           whiteSpace
           return $ List f

parseStatementList :: Parser [CVal]
parseStatementList = do
    s <- parseStatement
    ((:) s <$> (whiteSpace *> parseStatementList)) <|> pure (s : [])
    
parseStatement :: Parser CVal
parseStatement =
    try (do whiteSpace >> semi
            return (NullExp ()))
    <|> try (do exp <- parseExpression
                whiteSpace >> semi
                return (List exp))
    <|> try (do cs <- parseCompoundStatement
		return cs)

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

parseParameterTypeList :: Parser [CVal]
parseParameterTypeList = do
    p <- parseParameterDeclaration
    ((:) p <$> (whiteSpace *> comma *> whiteSpace *> parseParameterTypeList)) <|> pure (p : [])
    
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

parseCompoundStatement :: Parser CVal
parseCompoundStatement =
    try (do whiteSpace
            cs <- braces parseDeclarationList
            return (CompoundStatement cs []))
    <|> try (do whiteSpace
                cs <- braces parseStatementList
                return (CompoundStatement [] cs))
    <|> try (do whiteSpace >> char '{' >> whiteSpace
                dl <- try parseDeclarationList
                whiteSpace
                sl <- try parseStatementList
                whiteSpace >> char '}'
                return $ CompoundStatement dl sl)
    <|> do whiteSpace >> char '{' >> whiteSpace >> char '}'
           return $ CompoundStatement [] []

parseNumber :: Parser CVal
parseNumber = liftM (Number . read) $ many1 digit

test :: Parser CVal -> IO ()
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
   input <- getLine
   print $ case parse parseStatement "TinyC" input of
      Left err -> show err
      Right val -> show val
