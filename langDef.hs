--Language Definition

-- Doesn't work properly--cannot recognize relational expressions.

module LangDef where

import System.IO
import Control.Monad
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token
import Debug.Trace

type Parser = Parsec String ()

data Type   = IntType
            | BoolType
            -- | StringType
            deriving (Show, Eq)

data Value = IntValue Integer Type
           | BoolValue Bool Type
           -- | StringValue String
           deriving (Show)

instance Eq Value where
    IntValue i1 t1 == IntValue i2 t2 = ((i1 == i2) && (t1 == t2))
    BoolValue b1 t1 == BoolValue b2 t2 = ((b1 == b2) && (t1 == t2))
    _ == _ = False

instance Ord Value where
    compare (IntValue i1 _) (IntValue i2 _) = compare i1 i2
    compare (BoolValue b1 _) (BoolValue b2 _) = compare b1 b2
    compare (BoolValue _ _) (IntValue _ _) = LT
    compare (IntValue _ _) (BoolValue _ _) = GT

-- data ExprBool = ValBool Value
--               | Not ExprBool
--               | BinBool OpBool Expr Expr
--               | BinComb OpComb ExprBool ExprBool
--               deriving (Show)
-- Can define Expr

-- data OpBool = EqualTo
--             | GreaterThan
--             | LessThan
--             deriving (Show)

-- data OpComb = And
--             | Or
--             deriving (Show)

data Expr   = Var String
            | Val Value
            | Negate Expr
            | Comb OpComb Expr Expr
            | FuncCall String [Expr]
            deriving (Show)

data OpComb = EqualTo
            | GreaterThan
            | LessThan
            | And
            | Or
            | Add
            | Sub
            | Mult
            | Div
            deriving (Show)

data Stmt = Seq Stmt Stmt
          | Assign String Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
          | Func String [String] Stmt
          | ExprStmt Expr
          | Skip
          deriving (Show)

-- data Block = Seq Block Block
--            | Func String [String] Stmt

languageDef =
    emptyDef { Token.identStart      = letter
             , Token.identLetter     = alphaNum
             , Token.commentLine     = "//"
             , Token.reservedNames   = [ "if"
                                       , "then"
                                       , "def"
                                       , "call"
                                       , "begin"
                                       , "end"
                                       -- , "output"
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

-- Defining Block Parsers
-- block :: Parser Block
-- block = seqBlock
-- seqBlock :: Parser Block
-- seqBlock = do
--     list <- (sepBy1 funcStmt whiteSpace)
--     if length list == 1 then (return $ head list) else (return $ chain list)
-- TODO: Do we want to get rid of global statements, and force everything
-- to be inside a function?

-- Defining Statement Parsers
statement :: Parser Stmt
statement =  parens statement
          <|> seqStmt
statement' :: Parser Stmt
statement' =   ifStmt
           <|> whileStmt
           <|> skipStmt
           <|> assignStmt
           <|> funcStmt
           <|> exprStmt
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
    cond <- aExpression
    reserved "then"
    stmt1 <- statement
    reserved "else"
    stmt2 <- statement
    reserved "end"
    return $ If cond stmt1 stmt2
whileStmt :: Parser Stmt
whileStmt = do
    reserved "while"
    cond <- aExpression
    reserved "do"
    stmt <- statement
    reserved "end"
    return $ While cond stmt
assignStmt :: Parser Stmt
assignStmt = do
    var <- identifier
    reservedOp "="
    expr <- aExpression
    return $ Assign var expr
skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip
funcStmt :: Parser Stmt
funcStmt = do
    reservedOp "def"
    name <- identifier
    args <- (sepBy identifier whiteSpace)
    reservedOp "begin"
    stmts <- statement
    reservedOp "end"
    return $ Func name args stmts
    -- Perhaps we only need the number of arguments?
exprStmt :: Parser Stmt
exprStmt = do
    expr <- aExpression
    return $ ExprStmt expr


-- Defining Value Parsers
-- In-order precendence of the different values.
value :: Parser Value
value =   (reserved "true"  >> return (BoolValue True BoolType))
      <|> (reserved "false" >> return (BoolValue False BoolType))
      <|> liftM (`IntValue` IntType) integer
      -- See liftM documentation. This makes IntType the second argument to IntValue,
      -- letting liftM add the first argument.
      -- <|> liftM StringValue stringLiteral

-- Defining Expression Parsers
aExpression :: Parser Expr
aExpression =   buildExpressionParser aOperators aTerm
            <|> parens aExpression
            <|> funcCallStmt
    -- TODO: USe this instead.
    -- (\lhs _ op _ rhs -> )  <$> aExpression <* pSpace <*> relOp <*> pSpace <*> aExpression
     -- <$>
funcCallStmt :: Parser Expr
funcCallStmt = do
    reservedOp "call"
    name <- identifier
    args <- sepBy aTerm whiteSpace
    reservedOp "end"
    return $ FuncCall name args
-- bExpression :: Parser ExprBool
-- bExpression = buildExpressionParser bOperators bTerm
-- Define lists for operator precedence
aOperators = [ [ Prefix (reservedOp "-" >> return (Negate    ))
               , Prefix (reservedOp "not" >> return (Negate   ))          ]
             , [ Infix  (reservedOp "*" >> return (Comb Mult )) AssocLeft
               , Infix  (reservedOp "/" >> return (Comb Div  )) AssocLeft
               , Infix  (reservedOp "and" >> return (Comb And )) AssocLeft
               , Infix  (reservedOp "or"  >> return (Comb Or  )) AssocLeft
               ]
             , [ Infix  (reservedOp "+" >> return (Comb Add )) AssocLeft
               , Infix  (reservedOp "-" >> return (Comb Sub )) AssocLeft
               ]
             , [ Infix (reservedOp "==" >> return (Comb EqualTo)) AssocLeft
               , Infix (reservedOp ">" >> return (Comb GreaterThan)) AssocLeft
               , Infix (reservedOp "<" >> return (Comb LessThan)) AssocLeft ]
             ]
-- bOperators = [ [ Prefix (reservedOp "not" >> return (Negate   ))          ]
--              , [ Infix  (reservedOp "and" >> return (Comb And )) AssocLeft
--                , Infix  (reservedOp "or"  >> return (Comb Or  )) AssocLeft
--                ]
--              ]
-- Terms, used in Expressions
aTerm :: Parser Expr
aTerm =   parens aExpression
      <|> liftM Var identifier
      <|> liftM Val value
-- bTerm =   parens bExpression
--       <|> liftM ValBool value
--       <|> rExpression
-- rExpression :: Parser Expr
-- rExpression = trace "rExpression" $ do
--     a1 <- aExpression
--     op <- relation
--     a2 <- aExpression
--     return $ Comb op a1 a2
-- relation =   (reservedOp "==" >> return EqualTo)
--          <|> (reservedOp ">" >> return GreaterThan)
--          <|> (reservedOp "<" >> return LessThan)

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
