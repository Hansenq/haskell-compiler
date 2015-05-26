
module LangEval where

import LangDefOrig
import qualified Data.Map as Map

type Valuation = Map.Map String Value

-- TODO: Should this return (Val, Valuation)?
eval :: Stmt -> Valuation -> (Value, Valuation)
eval s env = case s of
    Seq st1 st2 ->      let (_, env') = eval st1 env
                        in  eval st2 env'
    -- Stmts sts ->        foldl (\env' s' -> eval s' env') env sts
    Assign x expr ->    let val = evalExprA expr env
                        in  (val, Map.insert x val env)
    If expr st1 st2 ->  let val = evalExprA expr env
                        in  case val of
                            (BoolValue True) ->     eval st1 env
                            (BoolValue False) ->    eval st2 env
    While expr st ->    let val = evalExprA expr env
                        in  case val of
                            (BoolValue True) ->     let (_, env') = eval st env
                                                    in  eval (While expr st) env'
                            (BoolValue False) ->    (BoolValue True, env)
                            -- Return true after while loops
    Skip  ->            (BoolValue False, env)

evalExprA :: ExprArith -> Valuation -> Value
evalExprA expr env = case expr of
    Var str ->              env Map.! str
    ValArith val ->         val
    Negate expr' ->         valNegate (evalExprA expr' env)
    ArithComb op ex1 ex2 -> let val1 = evalExprA ex1 env
                                val2 = evalExprA ex2 env
                            in  valArithComb op val1 val2

valNegate :: Value -> Value
valNegate (IntValue val)  = IntValue (- val)
valNegate (BoolValue val) = BoolValue (not val)
valNegate v               = v
-- TODO: Add Function Negation

valArithComb :: OpArith -> Value -> Value -> Value
valArithComb EqualTo v1 v2 = case (v1, v2) of
    (IntValue i1, IntValue i2)      -> BoolValue (i1 == i2)
    (BoolValue b1, BoolValue b2)    -> BoolValue (b1 == b2)
    (IntValue i1, BoolValue b1)     -> BoolValue (equateIntBool i1 b1)
    (BoolValue b1, IntValue i1)     -> BoolValue (equateIntBool i1 b1)
valArithComb GreaterThan (IntValue i1) (IntValue i2) = BoolValue (i1 > i2)
valArithComb LessThan (IntValue i1) (IntValue i2) = BoolValue (i1 < i2)
valArithComb Add (IntValue i1) (IntValue i2) = IntValue (i1 + i2)
valArithComb Sub (IntValue i1) (IntValue i2) = IntValue (i1 - i2)
valArithComb Mult (IntValue i1) (IntValue i2) = IntValue (i1 * i2)
valArithComb Div (IntValue i1) (IntValue i2) = IntValue (i1 `div` i2)
valArithComb _ _ _ = error "Incorrect Types"

valBoolComb :: OpBool -> Value -> Value -> Value
valBoolComb And (BoolValue b1) (BoolValue b2) = BoolValue (b1 && b2)
valBoolComb Or (BoolValue b1) (BoolValue b2) = BoolValue (b1 || b2)
valArithComb _ _ _ = error "Incorrect Types"

equateIntBool :: Integer -> Bool -> Bool
equateIntBool i b = case i of
    0 -> b == False
    _ -> b == True
