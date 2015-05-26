--Language Definition

module LangDefOrig where

import System.IO
import Control.Monad
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

type Parser = Parsec String ()

data Value = IntValue Integer
           | BoolValue Bool
           | StringValue String
           | FuncValue Stmt
           deriving (Show)

data ExprBool = ValBool Value
              | Not ExprBool
              | BinBool OpArith ExprArith ExprArith
              | BinComb OpBool ExprBool ExprBool
              deriving (Show)
-- Can define ExprArith

-- data OpBool = EqualTo
--             | GreaterThan
--             | LessThan
--             deriving (Show)

data OpBool = And
            | Or
            deriving (Show)

data ExprArith = Var String
               | ValArith Value
               | Negate ExprArith
               | ArithComb OpArith ExprArith ExprArith
               deriving (Show)

data OpArith = Add
             | Sub
             | Mult
             | Div
             | EqualTo
             | GreaterThan
             | LessThan
             deriving (Show)

data Stmt = Seq Stmt Stmt -- or Seq Stmt Stmt, makes more conceptual sense
          | Assign String ExprArith
          | If ExprBool Stmt Stmt
          | While ExprBool Stmt
          | Skip
          deriving (Show)

languageDef =
    emptyDef { Token.identStart      = letter
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
                                       ]
             , Token.reservedOpNames = ["+", "-", "*", "/", "=", "=="
                                       , "<", ">", "and", "or", "not"
                                       ]
    }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
stringLiteral = Token.stringLiteral lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

parser :: Parser Stmt
parser = whiteSpace >> statement

-- Defining Statement Parsers
statement :: Parser Stmt
statement =  parens statement
          <|> seqStmt
statement' :: Parser Stmt
statement' =   ifStmt
           <|> whileStmt
           <|> assignStmt
           <|> skipStmt
chain :: [Stmt] -> Stmt
chain (x:[]) = x
chain s = foldr1 (\x acc -> Seq x acc) s
-- chain (x:xs) = (Seq x (chain xs))
seqStmt :: Parser Stmt
seqStmt = do
    list <- (sepBy1 statement' whiteSpace)
    if length list == 1
        then
            return $ head list
        else
            -- TODO: Perhaps change this so we go straight to a chain, not a
            -- list, then a chain?
            return $ chain list
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
    reservedOp "="
    expr <- aExpression
    return $ Assign var expr
skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip

-- Defining Value Parsers
-- In-order precendence of the different values.
value :: Parser Value
value =   (reserved "true"  >> return (BoolValue True ))
      <|> (reserved "false" >> return (BoolValue False))
      <|> liftM IntValue integer
      <|> liftM StringValue stringLiteral
      <|> liftM FuncValue statement

-- Defining Expression Parsers
aExpression :: Parser ExprArith
aExpression = buildExpressionParser aOperators aTerm
bExpression :: Parser ExprBool
bExpression = buildExpressionParser bOperators bTerm
-- Define lists for operator precedence
aOperators = [ [ Prefix (reservedOp "-" >> return (Negate            ))         ]
             , [ Infix  (reservedOp "*" >> return (ArithComb Mult)) AssocLeft
               , Infix  (reservedOp "/" >> return (ArithComb Div  )) AssocLeft
               ]
             , [ Infix  (reservedOp "+" >> return (ArithComb Add     )) AssocLeft
               , Infix  (reservedOp "-" >> return (ArithComb Sub)) AssocLeft
               ]
             ]
bOperators = [ [ Prefix (reservedOp "not" >> return (Not             ))          ]
             , [ Infix  (reservedOp "and" >> return (BinComb And     )) AssocLeft
               , Infix  (reservedOp "or"  >> return (BinComb Or      )) AssocLeft
               ]
             ]
-- Terms, used in Expressions
aTerm =   parens aExpression
      <|> liftM Var identifier
      <|> liftM ValArith value
bTerm =   parens bExpression
      <|> liftM ValBool value
      <|> rExpression
rExpression = do
    a1 <- aExpression
    op <- relation
    a2 <- aExpression
    return $ BinBool op a1 a2
relation =   (reservedOp "==" >> return EqualTo)
         <|> (reservedOp ">" >> return GreaterThan)
         <|> (reservedOp "<" >> return LessThan)

parseString :: String -> Stmt
parseString str =
    case parse parser "" str of
        Left e  -> error $ show e
        Right r -> r

parseFile :: String -> IO Stmt
parseFile file = do
        program  <- readFile file
        case parse parser "" program of
            Left e  -> print e >> fail "parse error"
            Right r -> return r
