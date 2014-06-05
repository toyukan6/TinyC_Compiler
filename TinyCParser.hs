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
import System.IO
import Syntax.AST
import Syntax.Type

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

parseLogicalORExpr    :: Parser CVal
parseLogicalORExpr    = buildExpressionParser table parseFactor
    <?> "LogialORExpr"

table   = [[op "*" Mul AssocLeft, op "/" Div AssocLeft, op "%" Mod AssocLeft]
          ,[op "+" Add AssocLeft, op "-" Sub AssocLeft]
          ,[op "<" More AssocLeft, op ">" Less AssocLeft, op "<=" MoreE AssocLeft, op ">=" LessE AssocLeft]
          ,[op "==" Equal AssocLeft, op "!=" NEqual AssocLeft]
          ,[op "&&" L_AND AssocLeft]
	  ,[op "||" L_OR AssocLeft]
          ]
        where
          op s f assoc
             = Infix (do{ string s; return f}) assoc

parseExpression :: Parser CVal
parseExpression = do assign <- parseAssignExpr `sepBy1` comma
                     return (f assign)
		     <?> "Expression"
    where f (x : []) = x
          f (x : xs) = CValList x (f xs)
	  f [] = error "Bugs!"

parseAssignExpr :: Parser CVal
parseAssignExpr = try parseLogicalORExpr
                  <|> parseSubstitution
    <?> "AssignExpr"

parseSubstitution :: Parser CVal
parseSubstitution = do
    whiteSpace
    i <- parseDeclarator
    whiteSpace >> char '=' >> whiteSpace
    r <- parseAssignExpr
    return $ Assign i r
    <?> "Substitution"

parseUnaryExpr :: Parser CVal
parseUnaryExpr = try parsePostfixExpr
    <|> do whiteSpace >> char '-'
           ue <- parseUnaryExpr
	   whiteSpace
	   return $ Minus ue
    <?> "UnaryExpr"

parsePostfixExpr :: Parser CVal
parsePostfixExpr = try parseFactor
                   <|> parseCalFunc
    <?> "PostfixExpr"

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
    <?> "CalFunc"

parseArgumentExpressionList :: Parser [CVal]
parseArgumentExpressionList = do
    whiteSpace
    ae <- parseAssignExpr
    ((:) ae <$> (whiteSpace *> comma *> whiteSpace *> parseArgumentExpressionList)) <|> pure (ae : [])
    <?> "ArgumentExpressionList"

parseFactor :: Parser CVal
parseFactor = do
    whiteSpace
    f <- parseVar
    whiteSpace
    return f
    <|> do whiteSpace
           f <- try (parens parseExpression)
           whiteSpace
           return f
    <?> "Factor"

parseStatementList :: Parser [Statement]
parseStatementList = do
    s <- many parseStatement
    return s
    <?> "StatementList"
    
parseStatement :: Parser Statement
parseStatement =
    try (do whiteSpace >> semi
            return NullExp)
    <|> try (do exp <- parseExpression
                whiteSpace >> semi
                return (Expression exp))
    <|> try parseIf
    <|> try parseWhile
    <|> try (do cs <- parseCompoundStatement
		return cs)
    <?> "Statement"

parseIf :: Parser Statement
parseIf = do
    reserved "if"
    cond <- parens parseExpression
    state <- parseStatement
    (If cond state <$> (reserved "else" *> parseStatement)) <|> pure (If cond state NullExp)

parseWhile :: Parser Statement
parseWhile = do
    reserved "while"
    cond <- parens parseExpression
    state <- parseStatement
    return $ While cond state

{-    
parseProgram :: Parser CVal
parseProgram = do
    whiteSpace
    exp <- parseStatement
    whiteSpace
    return exp
    <?> "Program"
-}

parseVar :: Parser CVal
parseVar = do n <- parseNumber
              return n
	   <|> do i <- parseIdentifier
	          return $ Ident i
	   <?> "Var"    

parseIdentifier :: Parser Identifier
parseIdentifier = do name <- identifier
		     return $ Identifier name
		  <?> "Identifier"
	   
parseParameterTypeList :: Parser Statement
parseParameterTypeList = do
    p <- parseParameterDeclaration `sepBy1` comma
    return $ Declaration p
    <?> "ParameterTypeList"
    
parseParameterDeclaration :: Parser Variation
parseParameterDeclaration = do
    whiteSpace
    reserved "int"
    p <- parseDeclarator
    return p
    <?> "ParameterDeclaration"    

parseDeclarationList :: Parser [Statement]
parseDeclarationList = do
    d <- many parseDeclaration
    return d
    <?> "DeclarationList"
    
parseDeclaration :: Parser Statement
parseDeclaration = do
    reserved "int"
    pl <- parseDeclaratorList
    whiteSpace >> semi
    return pl
    <?> "Declaration"
    
parseDeclarator :: Parser Variation
parseDeclarator = do
    v <- parseIdentifier
    return $ Variation Int v
    <?> "Declarator"
    
parseDeclaratorList :: Parser Statement
parseDeclaratorList = do
    p <- parseDeclarator `sepBy1` comma
    return $ Declaration p
    <?> "DeclaratorList"

parseCompoundStatement :: Parser Statement
parseCompoundStatement =
    do symbol "{"
       dl <- parseDeclarationList
       sl <- parseStatementList
       symbol "}"
       return $ CompoundStatement $ dl ++ sl
    <?> "CompoundStatement"

parseNumber :: Parser CVal
parseNumber = liftM (Number . read) $ many1 digit

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
   input <- getLine
   print $ case parse parseStatement "TinyC" input of
      Left err -> show err
      Right val -> show val
