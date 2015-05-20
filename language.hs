--Language

module ParseLang where

import System.IO
import Control.Monad
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

type Parser = Parsec String ()

data ExprBool = BoolConst Bool
              | Not ExprBool
              | BinBool OpBool ExprArith ExprArith
              | BinComb OpComb ExprBool ExprBool
              deriving (Show)
-- Can define ExprArith

data OpBool = EqualTo
            | GreaterThan
            | LessThan
            deriving (Show)

data OpComb = And
            | Or
            deriving (Show)

data ExprArith = Var String
               | Number Integer
               | Negate ExprArith
               | ArithComb OpArith ExprArith ExprArith
               deriving (Show)

data OpArith = Add
             | Sub
             | Mult
             | Div
             deriving (Show)

data Stmt = Stmts [Stmt] -- or Seq Stmt Stmt, makes more conceptual sense
          | Assign String ExprArith
          | If ExprBool Stmt Stmt
          | While ExprBool Stmt
          | Continue
          deriving (Show)

languageDef =
    emptyDef { Token.identStart      = letter
             , Token.identLetter     = alphaNum
             , Token.reservedNames   = [ "if"
                                       , "then"
                                       , "else"
                                       , "while"
                                       , "do"
                                       , "continue"
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
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

parser :: Parser Stmt
parser = whiteSpace >> statement

statement :: Parser Stmt
statement =   parens statement
          <|> sequenceOfStmt
statement' :: Parser Stmt
statement' =   ifStmt
           <|> whileStmt
           <|> assignStmt
           <|> continueStmt
sequenceOfStmt = do
    list <- (sepBy1 statement' whiteSpace)
    return $ if length list == 1 then head list else Stmts list
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
continueStmt :: Parser Stmt
continueStmt = reserved "continue" >> return Continue

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
-- Terms!
aTerm =   parens aExpression
      <|> liftM Var identifier
      <|> liftM Number integer
bTerm =   parens bExpression
      <|> (reserved "true"  >> return (BoolConst True ))
      <|> (reserved "false" >> return (BoolConst False))
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
