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
spaces1 :: Parser ()
spaces1 = skipMany1 space

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

parseExpression :: Parser [CVal]
parseExpression = do
    assign <- parseAssignExpr
    ((:) assign <$> (whiteSpace *> comma *> whiteSpace *> parseExpression)) <|> pure (assign : [])
    <?> "Expression"

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
           return $ List f
    <?> "Factor"

parseStatementList :: Parser [CVal]
parseStatementList = do
    s <- parseStatement
    ((:) s <$> (whiteSpace *> parseStatementList)) <|> pure (s : [])
    <?> "StatementList"
    
parseStatement :: Parser CVal
parseStatement =
    try (do whiteSpace >> semi
            return (NullExp ()))
    <|> try (do exp <- parseExpression
                whiteSpace >> semi
                return (List exp))
    <|> try (do cs <- parseCompoundStatement
		return cs)
    <?> "Statement"
    
parseProgram :: Parser CVal
parseProgram = do
    whiteSpace
    exp <- parseStatement
    whiteSpace
    return exp
    <?> "Program"
    
parseVar :: Parser CVal
parseVar = do 
    whiteSpace
    n <- parseNumber
         <|> parseDeclarator
    whiteSpace
    return n
    <?> "Var"    

parseParameterTypeList :: Parser [CVal]
parseParameterTypeList = do
    p <- parseParameterDeclaration
    ((:) p <$> (whiteSpace *> comma *> whiteSpace *> parseParameterTypeList)) <|> pure (p : [])
    <?> "ParameterTypeList"
    
parseParameterDeclaration :: Parser CVal
parseParameterDeclaration = do
    whiteSpace
    char 'i' >> char 'n' >> char 't' >> spaces1
    p <- parseDeclarator
    return p
    <?> "ParameterDeclaration"    

parseDeclarationList :: Parser [CVal]
parseDeclarationList = do
    d <- parseDeclaration
    ((++) d <$> (whiteSpace *> parseDeclarationList)) <|> pure d
    <?> "DeclarationList"
    
parseDeclaration :: Parser [CVal]
parseDeclaration = do
    char 'i' >> char 'n' >> char 't'
    spaces1
    pl <- parseDeclaratorList
    whiteSpace >> semi
    return pl
    <?> "Declaration"
    
parseDeclarator :: Parser CVal
parseDeclarator = do
    v <- identifier
    return $ Variation Int v
    <?> "Declarator"
    
parseDeclaratorList :: Parser [CVal]
parseDeclaratorList = do
    p <- parseDeclarator
    ((:) p <$> (whiteSpace *> comma *> whiteSpace *> parseDeclaratorList)) <|> pure (p : [])
    <?> "DeclaratorList"

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
    <?> "CompoundStatement"

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
