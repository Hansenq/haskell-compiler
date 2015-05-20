
module ParseWhile where

import System.IO
import Control.Monad
--import Text.Parsec.Token
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

type Parser = Parsec String ()

data BExpr  = BoolConst Bool
            | Not BExpr
            | BBinary BBinOp BExpr BExpr
            | RBinary RBinOp AExpr AExpr
            deriving (Show)

data BBinOp = And | Or deriving (Show)

data RBinOp = Greater | Less deriving (Show)

data AExpr  = Var String
            | IntConst Integer
            | Neg AExpr
            | ABinary ABinOp AExpr AExpr
            -- | FuncCall String Args
            deriving (Show)

data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
            deriving (Show)

data Stmt   = Seq [Stmt]
            | Assign String AExpr
            | If BExpr Stmt Stmt
            | While BExpr Stmt
            | Skip
            -- | Func String Args Stmt
            deriving (Show)

-- data Args = SeqA [AExpr] deriving (Show)

languageDef =
    emptyDef { Token.commentStart    = "/*"
             , Token.commentEnd      = "*/"
             , Token.commentLine     = "//"
             , Token.identStart      = letter
             , Token.identLetter     = alphaNum
             , Token.reservedNames   = [ "if"
                                       , "then"
                                       , "else"
                                       , "while"
                                       , "do"
                                       , "skip"
                                       , "true"
                                       , "false"
                                       , "not"
                                       , "and"
                                       , "or"
                                       -- , "func"
                                       ]
             , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                       , "<", ">", "and", "or", "not"
                                       ]
    }

lexer = Token.makeTokenParser languageDef

-- Now we can do this:
identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer
commaSep = Token.commaSep lexer
comma = Token.comma lexer
-- etc. etc.

-- Remove whitespace at beginning
whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

-- Parser a statement. Use "sepBy1" to parser statements that are separated by ';'
statement :: Parser Stmt
statement =   parens statement
          <|> sequenceOfStmt -- used to express choice. a <|> b means that a is
                             -- used first; if it fails without consuming input,
                             -- then b is used.
                             -- If it will fail after consuming input, use "try"
statement' :: Parser Stmt
statement' =   ifStmt
           <|> whileStmt
           <|> skipStmt
           <|> assignStmt
sequenceOfStmt = do
    list <- (sepBy1 statement' semi)
    return $ if length list == 1 then head list else Seq list
    -- If there's only one statement, return it without using Seq

-- argument :: Parser Args
-- argument =   parens argument
--          <|> sequenceOfArg
-- sequenceOfArg = do
--     list <- (commaSep aExpression)
--     return $ SeqA list

-- Parsers for all of the possible statements
ifStmt :: Parser Stmt
ifStmt = do
    reserved "if"
    cond <- bExpression
    reserved "then"
    stmt1 <- statement
    reserved "else"
    stmt2 <- statement
    return $ If cond stmt1 stmt2
whileStmt :: Parser Stmt
whileStmt = do
    reserved "while"
    cond <- bExpression
    reserved "do"
    stmt <- statement
    return $ While cond stmt
assignStmt :: Parser Stmt
assignStmt = do
    var <- identifier
    reservedOp ":="
    expr <- aExpression
    return $ Assign var expr
skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip
-- funcStmt :: Parser Stmt
-- funcStmt = do
--     reserved "func"
--     fName <- identifier
--     args <- argument
--     reserved "do"
--     stmt <- statement
--     return $ Func fName args stmt



-- Parse the expressions
aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm
bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm
-- Define lists for operator precedence
aOperators = [ [ Prefix (reservedOp "-" >> return (Neg             ))         ]
             , [ Infix  (reservedOp "*" >> return (ABinary Multiply)) AssocLeft
               , Infix  (reservedOp "/" >> return (ABinary Divide  )) AssocLeft
               ]
             , [ Infix  (reservedOp "+" >> return (ABinary Add     )) AssocLeft
               , Infix  (reservedOp "-" >> return (ABinary Subtract)) AssocLeft
               ]
             ]
bOperators = [ [ Prefix (reservedOp "not" >> return (Not             ))          ]
             , [ Infix  (reservedOp "and" >> return (BBinary And     )) AssocLeft
               , Infix  (reservedOp "or"  >> return (BBinary Or      )) AssocLeft
               ]
             ]
-- Terms!
aTerm =   parens aExpression
      <|> liftM Var identifier
      <|> liftM IntConst integer
      -- <|> liftM FuncCall identifier argument
bTerm =   parens bExpression
      <|> (reserved "true"  >> return (BoolConst True ))
      <|> (reserved "false" >> return (BoolConst False))
      <|> rExpression
rExpression = do
    a1 <- aExpression
    op <- relation
    a2 <- aExpression
    return $ RBinary op a1 a2
relation =   (reservedOp ">" >> return Greater)
         <|> (reservedOp "<" >> return Less)


parseString :: String -> Stmt
parseString str =
    case parse whileParser "" str of
        Left e  -> error $ show e
        Right r -> r

parseFile :: String -> IO Stmt
parseFile file = do
        program  <- readFile file
        case parse whileParser "" program of
            Left e  -> print e >> fail "parse error"
            Right r -> return r