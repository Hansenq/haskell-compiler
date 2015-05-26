-- This doesn't work beacuse langDef.hs doesn't work properly.


module LangEval where

import LangDef
import qualified Data.Map as Map

type Valuation = Map.Map String Value

-- TODO: Should this return (Val, Valuation)?
eval :: Stmt -> Valuation -> (Value, Valuation)
eval s env = case s of
    Seq st1 st2 ->      let (_, env') = eval st1 env
                        in  eval st2 env'
    -- Stmts sts ->        foldl (\env' s' -> eval s' env') env sts
    Assign x expr ->    let val = evalExpr expr env
                        in  (val, Map.insert x val env)
    If expr st1 st2 ->  let val = evalExpr expr env
                        in  case val of
                            (BoolValue True) ->     eval st1 env
                            (BoolValue False) ->    eval st2 env
    While expr st ->    let val = evalExpr expr env
                        in  case val of
                            (BoolValue True) ->     let (_, env') = eval st env
                                                    in  eval (While expr st) env'
                            (BoolValue False) ->    (BoolValue True, env)
                            -- Return true after while loops
    Skip  ->            (BoolValue False, env)

evalExpr :: Expr -> Valuation -> Value
evalExpr expr env = case expr of
    Var str ->              env Map.! str
    Val val ->              val
    Negate expr' ->         valNegate (evalExpr expr' env)
    Comb op ex1 ex2 ->  let val1 = evalExpr ex1 env
                            val2 = evalExpr ex2 env
                        in  valComb op val1 val2

valNegate :: Value -> Value
valNegate (IntValue val)  = IntValue (- val)
valNegate (BoolValue val) = BoolValue (not val)
valNegate v               = v
-- TODO: Add Function Negation

valComb :: OpComb -> Value -> Value -> Value
valComb EqualTo v1 v2 = case (v1, v2) of
    (IntValue i1, IntValue i2)      -> BoolValue (i1 == i2)
    (BoolValue b1, BoolValue b2)    -> BoolValue (b1 == b2)
    (IntValue i1, BoolValue b1)     -> BoolValue (equateIntBool i1 b1)
    (BoolValue b1, IntValue i1)     -> BoolValue (equateIntBool i1 b1)
valComb GreaterThan (IntValue i1) (IntValue i2) = BoolValue (i1 > i2)
valComb LessThan (IntValue i1) (IntValue i2) = BoolValue (i1 < i2)
valComb And (BoolValue b1) (BoolValue b2) = BoolValue (b1 && b2)
valComb Or (BoolValue b1) (BoolValue b2) = BoolValue (b1 || b2)
valComb Add (IntValue i1) (IntValue i2) = IntValue (i1 + i2)
valComb Sub (IntValue i1) (IntValue i2) = IntValue (i1 - i2)
valComb Mult (IntValue i1) (IntValue i2) = IntValue (i1 * i2)
valComb Div (IntValue i1) (IntValue i2) = IntValue (i1 `div` i2)
valComb _ _ _ = error "Incorrect Types"


equateIntBool :: Integer -> Bool -> Bool
equateIntBool i b = case i of
    0 -> b == False
    _ -> b == True

evalFile :: String -> IO (Value, Valuation)
evalFile file = do
    stmt <- parseFile file
    print stmt
    return (eval stmt Map.empty)