module Parser.TinyCParser where

import Control.Monad.Error
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language

import Syntax.AST
import Syntax.Type

--TinyCの言語仕様
tinyCStyle = emptyDef {
	   commentStart   = "/*" --複数行コメントの開始
           , commentEnd     = "*/" --複数行コメントの終わり
           , commentLine    = "//" --コメントライン
           , nestedComments = True --複数行コメントを許す
           , identStart     = letter <|> char '_' --変数名の開始
           , identLetter    = alphaNum <|> oneOf "_" --変数名の利用可能文字
           , opStart        = opLetter emptyDef --演算子の開始
           , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~" --演算子の利用可能文字
           , reservedOpNames= [] --拒絶される演算子
           , reservedNames  = ["if", "else", "while", "return", "int", "void"] --拒絶される変数名
           , caseSensitive  = True --大文字と小文字を区別するか
           }

--tinyCStyleに則る
lexer :: Token.TokenParser ()
lexer  = Token.makeTokenParser tinyCStyle

--空白が1個以上のパース
whiteSpace :: Parser () 
whiteSpace = Token.whiteSpace lexer
--空白つきの何かのパース
lexeme     :: Parser a -> Parser a
lexeme     = Token.lexeme lexer  
--シンボルのパース
symbol     :: String -> Parser String
symbol     = Token.symbol lexer
--数字のパース
natural    :: Parser Integer
natural    = Token.natural lexer
--変数名のパース
identifier :: Parser String
identifier = Token.identifier lexer
--拒絶のパース
reserved   :: String -> Parser ()
reserved   = Token.reserved lexer
--演算子のパース
operator   :: Parser String
operator   = Token.operator lexer
--拒絶される演算子のパース
reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer
--かっこのパース
parens     :: Parser a -> Parser a
parens	      = Token.parens lexer
--波かっこのパース
braces     :: Parser a -> Parser a
braces	      = Token.braces lexer
--角かっこのパース
squares    :: Parser a -> Parser a
squares    = Token.squares lexer
--セミコロン
semi       :: Parser String
semi       = Token.semi lexer
--コンマ
comma      :: Parser String
comma      = Token.comma lexer

--logical-OR-exprからmult-exprまでのパース
parseLogicalORExpr    :: Parser CVal
parseLogicalORExpr    = buildExpressionParser table (lexeme parseUnaryExpr)
    <?> "LogialORExpr"

--演算子のパースとそのとき実行する関数のリスト
table   = [[op "*" Mul AssocLeft, op "/" Div AssocLeft, op "%" Mod AssocLeft]
          ,[op "+" Add AssocLeft, op "-" Sub AssocLeft]
          ,[op "<" Less AssocLeft, op ">" More AssocLeft, op "<=" LessE AssocLeft, op ">=" MoreE AssocLeft]
          ,[op "==" Equal AssocLeft, op "!=" NEqual AssocLeft]
          ,[op "&&" L_AND AssocLeft]
	  ,[op "||" L_OR AssocLeft]
          ]
        where op s f assoc = Infix (do{ reservedOp s; return f}) assoc

--expressionのパース
parseExpression :: Parser CVal
parseExpression = do whiteSpace
		     assign <- (lexeme parseAssignExpr) `sepBy1` comma
                     return (f assign)
		     <?> "Expression"
    where f (x : []) = x
          f (x : xs) = CValList x (f xs)
	  f [] = error "Bugs!"

--assign-exprのパース
parseAssignExpr :: Parser CVal
parseAssignExpr = try parseSubstitution
                  <|> parseLogicalORExpr
    <?> "AssignExpr"

--substitutionのパース
parseSubstitution :: Parser CVal
parseSubstitution = do
    i <- parseIdentifier
    reservedOp "="
    r <- parseAssignExpr
    return $ Assign i r
    <?> "Substitution"

--unary-exprのパース
parseUnaryExpr :: Parser CVal
parseUnaryExpr = parsePostfixExpr
    <|> do char '-' >> whiteSpace
           ue <- lexeme parseUnaryExpr
	   return $ Minus ue
    <?> "UnaryExpr"

--postfix-exprのパース
parsePostfixExpr :: Parser CVal
parsePostfixExpr = try parseCalFunc
                   <|> parsePrimaryExpr
    <?> "PostfixExpr"

--関数呼び出しのパース
parseCalFunc :: Parser CVal
parseCalFunc = do
    i <- lexeme parseIdentifier
    arl <- parens parseArgumentExpressionList
    return $ CalFunc i arl
    <?> "CalFunc"

--argument-expression-listのパース
parseArgumentExpressionList :: Parser [CVal]
parseArgumentExpressionList = parseAssignExpr `sepBy` comma
    <?> "ArgumentExpressionList"

--primary-exprのパース
parsePrimaryExpr :: Parser CVal
parsePrimaryExpr = parseVar
	      <|> parens parseExpression
    <?> "PrimaryExpr"

--statement-listのパース
parseStatementList :: Parser [Statement]
parseStatementList = many parseStatement
    <?> "StatementList"

--statementのパース
parseStatement :: Parser Statement
parseStatement =
    do lexeme semi
       return NullExp 
    <|> parseIf
    <|> parseWhile
    <|> parseReturn
    <|> do cs <- parseCompoundStatement
           return cs
    <|> do exp <- parseExpression
           lexeme semi
           return $ Expression exp
    <?> "Statement"

--if文のパース
parseIf :: Parser Statement
parseIf = do
    reserved "if"
    cond <- parens parseExpression
    state <- parseStatement
    elsestate <- parseElse
    return $ If cond state elsestate

--else節のパース
parseElse :: Parser Statement
parseElse = do
    reserved "else"
    state <- parseStatement
    return state
    <|> return NullExp

--while文のパース
parseWhile :: Parser Statement
parseWhile = do
    reserved "while"
    cond <- parens parseExpression
    state <- parseStatement
    return $ While cond state

--return文のパース
parseReturn :: Parser Statement
parseReturn = do
    reserved "return"
    exp <- optionMaybe parseExpression
    lexeme semi
    return $ Return exp

--programのパース
parseProgram :: Parser [Program]
parseProgram = many parseExternalDeclaration
    <?> "Program"

--external-declarationのパース
parseExternalDeclaration :: Parser Program
parseExternalDeclaration = try (do dec <- parseDeclaration
			           return (PDecl dec))
    <|> do func <- parseFunctionDefinition
           return $ PFunc func

--数字または識別子のパース
parseVar :: Parser CVal
parseVar = parseNumber
           <|> do i <- parseIdentifier
	          return $ Ident i
	   <?> "Var"    

--識別子のパース
parseIdentifier :: Parser Identifier
parseIdentifier = do name <- identifier
		     return $ Identifier name
		  <?> "Identifier"

--function-definitionのパース
parseFunctionDefinition :: Parser Function
parseFunctionDefinition = do
    reserved "int"
    name <- parseIdentifier
    param <- parens parseParameterTypeList
    body <- parseCompoundStatement
    return $ Func CInt name param body

--parameter-type-listのパース
parseParameterTypeList :: Parser ParamDecl
parseParameterTypeList = do
    pd <- parseParameterDeclaration `sepBy` comma
    return $ ParamDecl pd
    <?> "ParameterTypeList"
    
--parameter-declarationのパース
parseParameterDeclaration :: Parser Variation
parseParameterDeclaration = do
    reserved "int"
    p <- parseDeclarator
    return p
    <?> "ParameterDeclaration"    

--declaration-listのパース
parseDeclarationList :: Parser [Statement]
parseDeclarationList = many parseDeclaration
    <?> "DeclarationList"
    
--declarationのパース
parseDeclaration :: Parser Statement
parseDeclaration = do
    reserved "int"
    pl <- parseDeclaratorList
    lexeme semi
    return pl
    <?> "Declaration"
    
--declaratorのパース
parseDeclarator :: Parser Variation
parseDeclarator = do
    v <- parseIdentifier
    return $ Variation CInt v
    <?> "Declarator"
    
--declarator-listのパース
parseDeclaratorList :: Parser Statement
parseDeclaratorList = do
    p <- parseDeclarator `sepBy1` comma
    return $ Declaration p
    <?> "DeclaratorList"

--compound-statementのパース
parseCompoundStatement :: Parser Statement
parseCompoundStatement =
    do symbol "{"
       dl <- parseDeclarationList
       sl <- parseStatementList
       symbol "}"
       return $ CompoundStatement $ dl ++ sl
    <?> "CompoundStatement"

--数字のパース
parseNumber :: Parser CVal
parseNumber = liftM (Number . read) $ many1 digit

--型のパース
parseType :: Parser Type
parseType = (reserved "int" >> return CInt)
            <|> (reserved "void" >> return CVoid)
	    <?> "Type"