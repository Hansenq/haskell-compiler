
module LangEval where

import LangDef
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Debug.Trace

type Valuation = Map.Map String Value
-- type Types = Map.Map String Type
-- Function: name, ([argNames], funcBody, Memoisation Map)
type FuncStorage = Map.Map String ([String], Stmt, Map.Map [Value] Value)

eval :: Stmt -> Valuation -> FuncStorage -> (Value, Valuation, FuncStorage)
eval s env fEnv = case s of
    Seq st1 st2 ->          let (_, env', fEnv') = eval st1 env fEnv
                            in  eval st2 env' fEnv'
    -- Stmts sts ->            foldl (\env' s' -> eval s' env') env sts
    Assign x expr ->        let (val, fEnv') = evalExpr expr env fEnv
                            in  (val, (Map.insert x val env), fEnv')
    If expr st1 st2 ->      let (val, fEnv') = evalExpr expr env fEnv
                            in  case val of
                                (BoolValue True BoolType) ->     eval st1 env fEnv'
                                (BoolValue False BoolType) ->    eval st2 env fEnv'
    While expr st ->        let (val, fEnv') = evalExpr expr env fEnv
                            in  case val of
                                (BoolValue True BoolType) ->    let (_, env', fEnv'') = eval st env fEnv'
                                                                in  eval (While expr st) env' fEnv''
                                (BoolValue False BoolType) ->   (newBool True, env, fEnv')
                                -- Return true after while loops
    Skip  ->                (BoolValue False BoolType, env, fEnv)
    -- Add in ExprStmt Expr
    Func name args st ->    (BoolValue True BoolType, env, (Map.insert name (args, st, Map.empty) fEnv))
    _ ->                    error "Statement not found."

-- State Monad
-- Create a data type called State, put FuncStorage and counters inside
evalExpr :: Expr -> Valuation -> FuncStorage -> (Value, FuncStorage)
evalExpr expr env fEnv = case expr of
    Var str ->              (env Map.! str, fEnv)
    Val val ->              (val, fEnv)
    Negate expr' ->         let (val, fEnv') = evalExpr expr' env fEnv
                                negVal = valNegate val
                            in  (negVal, fEnv')
    Comb op ex1 ex2 ->      let (val1, fEnv') = evalExpr ex1 env fEnv
                                (val2, fEnv'') = evalExpr ex2 env fEnv'
                            in  (valComb op val1 val2, fEnv'')
    FuncCall name args ->   let -- Convert Expr to Values
                                (argValues, fEnv') = foldl (\(list, fEnv') e -> let (val, fEnv'') = (evalExpr e env fEnv')
                                                                                 in  (list ++ [val], fEnv'')) ([], fEnv) args
                                -- Create local Map for function call. Add in arguments.
                                (argNames, stmt, cache) = fEnv' Map.! name
                                -- Memoisation
                                val = Map.lookup argValues cache
                            in  case val of
                                Just v ->   (v, fEnv')
                                Nothing ->  let (val, tempEnv', fEnv'') = eval stmt (Map.fromList (zip argNames argValues)) fEnv'
                                                -- Gets "output"; defaults to val
                                                output = Maybe.fromMaybe val (Map.lookup "output" tempEnv')
                                                -- Adds to previous cache
                                                (_, _, cache') = fEnv'' Map.! name
                                                map = Map.insert argValues output cache'
                                                -- Adds the function with updated cache back into fEnv''
                                                funcStorage = Map.insert name (argNames, stmt, map) fEnv''
                                            in  (output, funcStorage)

valNegate :: Value -> Value
valNegate (IntValue val _)  = newInt (- val)
valNegate (BoolValue val _) = newBool (not val)
-- valNegate v               = v -- Don't need this right now; shows error.
-- TODO: Add Function Negation

valComb :: OpComb -> Value -> Value -> Value
valComb EqualTo v1 v2 = case (v1, v2) of
    (IntValue i1 _, IntValue i2 _)      -> newBool (i1 == i2)
    (BoolValue b1 _, BoolValue b2 _)    -> newBool (b1 == b2)
    (IntValue i1 _, BoolValue b1 _)     -> newBool (equateIntBool i1 b1)
    (BoolValue b1 _, IntValue i1 _)     -> newBool (equateIntBool i1 b1)
valComb GreaterThan (IntValue i1 _) (IntValue i2 _) = newBool (i1 > i2)
valComb LessThan (IntValue i1 _) (IntValue i2 _) = newBool (i1 < i2)
valComb And (BoolValue b1 _) (BoolValue b2 _) = newBool (b1 && b2)
valComb Or (BoolValue b1 _) (BoolValue b2 _) = newBool (b1 || b2)
valComb Add (IntValue i1 _) (IntValue i2 _) = newInt (i1 + i2)
valComb Sub (IntValue i1 _) (IntValue i2 _) = newInt (i1 - i2)
valComb Mult (IntValue i1 _) (IntValue i2 _) = newInt (i1 * i2)
valComb Div (IntValue i1 _) (IntValue i2 _) = newInt (i1 `div` i2)
valComb op v1 v2 = error ("Incorrect Types: " ++ (show op) ++ " " ++ (show v1) ++ " " ++ (show v2))

newBool :: Bool -> Value
newBool bool = BoolValue bool BoolType

newInt :: Integer -> Value
newInt int = IntValue int IntType

equateIntBool :: Integer -> Bool -> Bool
equateIntBool i b = case i of
    0 -> b == False
    _ -> b == True

evalFile :: String -> IO (Value, Valuation, FuncStorage)
evalFile file = do
    stmt <- parseFile file
    print stmt
    let (val, env, fEnv) = eval stmt Map.empty Map.empty
    putStrLn $ "Output is: " ++ (show $ Maybe.fromMaybe val $ Map.lookup "output" env)
    return (val, env, fEnv)
