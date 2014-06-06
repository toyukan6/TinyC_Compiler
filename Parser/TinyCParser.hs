module Parser.TinyCParser where

import Control.Monad.Error
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language

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
parseLogicalORExpr    = buildExpressionParser table parseUnaryExpr
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
             = Infix (do{ reservedOp s; return f}) assoc

parseExpression :: Parser CVal
parseExpression = do assign <- parseAssignExpr `sepBy1` comma
                     return (f assign)
		     <?> "Expression"
    where f (x : []) = x
          f (x : xs) = CValList x (f xs)
	  f [] = error "Bugs!"

parseAssignExpr :: Parser CVal
parseAssignExpr = try parseSubstitution
                  <|> parseLogicalORExpr
    <?> "AssignExpr"

parseSubstitution :: Parser CVal
parseSubstitution = do
    whiteSpace
    i <- parseIdentifier
    whiteSpace >> reservedOp "=" >> whiteSpace
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
parsePostfixExpr = try parseCalFunc
                   <|> parseFactor
    <?> "PostfixExpr"

parseCalFunc :: Parser CVal
parseCalFunc = do
    i <- parseIdentifier
    arl <- parens parseArgumentExpressionList
    return $ CalFunc i arl
    <?> "CalFunc"

parseArgumentExpressionList :: Parser [CVal]
parseArgumentExpressionList = parseAssignExpr `sepBy` comma
    <?> "ArgumentExpressionList"

parseFactor :: Parser CVal
parseFactor = parseVar
	      <|> parens parseExpression
    <?> "Factor"

parseStatementList :: Parser [Statement]
parseStatementList = many parseStatement
    <?> "StatementList"
    
parseStatement :: Parser Statement
parseStatement =
    try (do whiteSpace >> semi
            return NullExp)
    <|> try (do exp <- parseExpression
                whiteSpace >> semi
                return (Expression exp))
    <|> try (do cs <- parseCompoundStatement
		return cs)
    <|> try parseIf
    <|> try parseWhile
    <|> try parseReturn
    <?> "Statement"

parseIf :: Parser Statement
parseIf = do
    reserved "if"
    cond <- parens parseExpression
    state <- parseStatement
    elsestate <- parseElse
    return $ If cond state elsestate

parseElse :: Parser Statement
parseElse = do
    reserved "else"
    state <- parseStatement
    return state
    <|> return NullExp

parseWhile :: Parser Statement
parseWhile = do
    reserved "while"
    cond <- parens parseExpression
    state <- parseStatement
    return $ While cond state

parseReturn :: Parser Statement
parseReturn = do
    reserved "return"
    exp <- optionMaybe parseExpression
    whiteSpace >> semi
    return $ Return exp

parseProgram :: Parser [Program]
parseProgram = parseExternalDeclaration `sepBy1` whiteSpace
    <?> "Program"

parseExternalDeclaration :: Parser Program
parseExternalDeclaration = try (do dec <- parseDeclaration
			           return (PDecl dec))
    <|> do func <- parseFunctionDefinition
           return $ PFunc func
    
parseVar :: Parser CVal
parseVar = parseNumber
           <|> do i <- parseIdentifier
	          return $ Ident i
	   <?> "Var"    

parseIdentifier :: Parser Identifier
parseIdentifier = do name <- identifier
		     return $ Identifier name
		  <?> "Identifier"

parseFunctionDefinition :: Parser Function
parseFunctionDefinition = do
    reserved "int"
    name <- parseIdentifier
    param <- parens parseParameterTypeList
    body <- parseCompoundStatement
    return $ Func CInt name param body
	   
parseParameterTypeList :: Parser [Variation]
parseParameterTypeList = parseParameterDeclaration `sepBy` comma
    <?> "ParameterTypeList"
    
parseParameterDeclaration :: Parser Variation
parseParameterDeclaration = do
    reserved "int"
    p <- parseDeclarator
    return p
    <?> "ParameterDeclaration"    

parseDeclarationList :: Parser [Statement]
parseDeclarationList = many parseDeclaration
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
    return $ Variation CInt v
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

parseType :: Parser Type
parseType = (reserved "int" >> return CInt)
            <|> (reserved "void" >> return CVoid)
	    <?> "Type"