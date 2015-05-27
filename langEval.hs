
module LangEval where

import LangDef
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

type Valuation = Map.Map String Value
-- type Types = Map.Map String Type
-- Function: name, ([argNames], funcBody, Memoisation Map)
type FuncStorage = Map.Map String ([String], Stmt, Map.Map [Value] Value)

eval :: Stmt -> Valuation -> FuncStorage -> (Value, Valuation, FuncStorage)
eval s env fEnv = case s of
    Seq st1 st2 ->          let (_, env', fEnv') = eval st1 env fEnv
                            in  eval st2 env' fEnv'
    -- Stmts sts ->            foldl (\env' s' -> eval s' env') env sts
    Assign x expr ->        let val = evalExpr expr env fEnv
                            in  (val, Map.insert x val env, fEnv)
    If expr st1 st2 ->      let val = evalExpr expr env fEnv
                            in  case val of
                                (BoolValue True) ->     eval st1 env fEnv
                                (BoolValue False) ->    eval st2 env fEnv
    While expr st ->        let val = evalExpr expr env fEnv
                            in  case val of
                                (BoolValue True) ->     let (_, env', fEnv') = eval st env fEnv
                                                        in  eval (While expr st) env' fEnv'
                                (BoolValue False) ->    (BoolValue True, env, fEnv)
                                -- Return true after while loops
    Skip  ->                (BoolValue False, env, fEnv)
    Func name args st ->    (BoolValue True, env, Map.insert name (args, st, Map.empty) fEnv)
    _ ->                    error "Statement not found."

evalExpr :: Expr -> Valuation -> FuncStorage -> Value
evalExpr expr env fEnv = case expr of
    Var str ->              env Map.! str
    Val val ->              val
    Negate expr' ->         valNegate (evalExpr expr' env fEnv)
    Comb op ex1 ex2 ->      let val1 = evalExpr ex1 env fEnv
                                val2 = evalExpr ex2 env fEnv
                            in  valComb op val1 val2
    FuncCall name args ->   let -- Convert Expr to Values
                                argValues = map (\e -> evalExpr e env fEnv) args
                                -- Create local Map for function call. Add in arguments.
                                (argNames, stmt, cache) = fEnv Map.! name
                                -- Memoisation
                                val = Map.lookup argValues cache
                            in  case val of
                                Just v ->   v
                                Nothing ->  let (val, tempEnv', _) = eval stmt (Map.fromList (zip argNames argValues)) fEnv
                                            in  Maybe.fromMaybe val (Map.lookup "output" tempEnv')

valNegate :: Value -> Value
valNegate (IntValue val)  = IntValue (- val)
valNegate (BoolValue val) = BoolValue (not val)
-- valNegate v               = v -- Don't need this right now; shows error.
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
valComb op v1 v2 = error ("Incorrect Types: " ++ (show op) ++ " " ++ (show v1) ++ " " ++ (show v2))


equateIntBool :: Integer -> Bool -> Bool
equateIntBool i b = case i of
    0 -> b == False
    _ -> b == True

evalFile :: String -> IO (Value, Valuation, FuncStorage)
evalFile file = do
    stmt <- parseFile file
    print stmt
    let (val, env, fEnv) = eval stmt Map.empty Map.empty
    putStrLn $ "Output is: " ++ (show $ env Map.! "output")
    return (val, env, fEnv)
