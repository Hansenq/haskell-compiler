{-#LANGUAGE RecordWildCards #-}
module LangEval where

import LangDef
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Debug.Trace
import Control.Monad.State.Strict
import Criterion.Main
-- Control.Monad.State.Lazy

type Valuation = Map.Map String Value
-- type Types = Map.Map String Type
-- Function: name, ([argNames], funcBody, Memoisation Map)
type FuncStorage = Map.Map String ([String], Stmt, Map.Map [Value] Value, (Integer, Integer))

-- Environment data type
data Env = Env { val :: Valuation, store :: FuncStorage, memoise :: Bool }
  deriving Show

-- State of the evaluator
type EvalState a = State Env a

-- Initial Environment
iEnv :: Env
iEnv = Env Map.empty Map.empty True

-- get :: State Env a -> State Env
eval' :: Stmt -> EvalState Value
eval' s =
    case s of
        Seq st1 st2 -> do
            eval' st1
            eval' st2
        Assign x expr -> do
            res <- evalExpr' expr   -- computing the value of the expression
            env@Env{..} <- get      -- getting the new environment after evalExpr.
                                    -- env is the env variable, while "@Env{..}"
                                    -- imports 'val' and 'store' variables
            let nenv = env { val = Map.insert x res val } -- update the valuation in env
            put nenv                -- puting the new environment with the updated valuation
            return res
        If expr st1 st2 -> do
            val <- evalExpr' expr
            case val of
                BoolValue True BoolType ->    eval' st1
                BoolValue False BoolType ->   eval' st2
                _ ->     error "IF case fallthrough"
        While expr st -> do
            val <- evalExpr' expr
            case val of
                BoolValue True BoolType ->  do
                    eval' st
                    eval' (While expr st)
                BoolValue False BoolType -> return $ BoolValue True BoolType
                _ ->     error "WHILE case fallthrough"
        Skip -> return $ BoolValue False BoolType
        _ ->    error "Statement not found"


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
    Func name args st ->    (BoolValue True BoolType, env, (Map.insert name (args, st, Map.empty, (0,0)) fEnv))
    _ ->                    error "Statement not found."

evalExpr' :: Expr -> EvalState Value
evalExpr' expr = case expr of
    Var str -> do
        env@Env{..} <- get
        return $ val Map.! str
    Val val -> return val
    Negate expr' -> do
        val <- evalExpr' expr
        return $ valNegate val
    FuncCall name args -> do
        env@Env{..} <- get
        let argValues = map (\arg -> undefined) args -- TODO: The lambda function in the map.E
            (argNames, stmt, cache, (countCalc, countMem)) = store Map.! name
            resMaybe = Map.lookup argValues cache
        undefined
        -- if resMaybe == (Just _) && memoise
        --     then -- Fetch from memoisation
        --         undefined
        --         -- Can't return here; must return at end of do block (and only one line)
        --     else
        --         undefined


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
                                (argNames, stmt, cache, (countCalc, countMem)) = fEnv' Map.! name
                                -- Memoisation
                                val = Map.lookup argValues cache
                            in
                            -- Comment out the next four lines to disable Memoisation.
                                case val of
                                Just v ->   let fEnv'' = Map.insert name (argNames, stmt, cache, (countCalc, countMem + 1)) fEnv'
                                            in  (v, fEnv'')
                                Nothing ->
                                            let (val, tempEnv', fEnv'') = eval stmt (Map.fromList (zip argNames argValues)) fEnv'
                                                -- Gets "output"; defaults to val
                                                output = Maybe.fromMaybe val (Map.lookup "output" tempEnv')
                                                -- Adds to previous cache
                                                (_, _, cache', (countCalc', countMem')) = fEnv'' Map.! name
                                                cache'' = Map.insert argValues output cache'
                                                -- Adds the function with updated cache back into fEnv''
                                                funcStorage = Map.insert name (argNames, stmt, cache'', (countCalc' + 1, countMem')) fEnv''
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

getStats :: FuncStorage -> [(String, (Integer, Integer))]
getStats fEnv = map (\(n, (_, _, _, s)) -> (n, s)) (Map.assocs fEnv)

calcStats :: [(String, (Integer, Integer))] -> String
calcStats lst = foldl (\acc (name, (calc, mem)) ->
                    acc ++ ("Function \"" ++ (show name) ++ "\": " ++ (show mem) ++ "/" ++ (show calc) ++ ", " ++ (show ((fromIntegral mem) / (fromIntegral (mem + calc)))))
                    ) "" lst

evalFile :: String -> IO (Value, Valuation, FuncStorage)
evalFile file = do
    stmt <- parseFile file
    putStrLn "### Raw ASN: "
    print stmt
    putStrLn ""
    let (val, env, fEnv) = eval stmt Map.empty Map.empty
        -- (val, Env env fEnv True) = runState (eval' stmt) iEnv
    putStrLn $ "### Output is: "
    print (Maybe.fromMaybe val $ Map.lookup "output" env)
    putStrLn ""
    let memStats = getStats fEnv
    putStrLn $ "### Memoisation Stats (% of calls that are memoised): "
    putStrLn $ calcStats memStats
    putStrLn ""
    return (val, env, fEnv)

evalWrapper :: Stmt -> Valuation -> FuncStorage -> Integer -> (Value, Valuation, FuncStorage)
evalWrapper stmt env fEnv num = eval stmt (Map.union (Map.singleton "num" (IntValue num IntType)) env) fEnv

testWrapper :: String -> Integer -> IO (Value, FuncStorage)
testWrapper file num = do
    stmt <- parseFile file
    let func = evalWrapper stmt Map.empty Map.empty
        (val, env, fEnv) = func num
    let memStats = getStats fEnv
    -- putStrLn $ "### Output is: "
    -- print (Maybe.fromMaybe val $ Map.lookup "output" env)
    -- putStrLn ""
    -- putStrLn $ "### Memoisation Stats (% of calls that are memoised): "
    -- putStrLn $ calcStats memStats
    -- putStrLn ""
    let (IntValue x IntType) = val
    putStrLn ((show num) ++ ":\t " ++ (show x))
    return (val, fEnv)


main = do
    putStrLn $ "Benchmarking: "
    stmt <- parseFile "fib1.hk"
    let toBench = evalWrapper stmt Map.empty Map.empty
    defaultMain [
        bgroup "fib" [ bench "3" $ whnfIO ((testWrapper "fib1.hk") 3)
                     , bench "4" $ whnfIO ((testWrapper "fib1.hk") 4)
                     , bench "5" $ whnfIO ((testWrapper "fib1.hk") 5)
                     , bench "7" $ whnfIO ((testWrapper "fib1.hk") 7)
                     , bench "10" $ whnfIO ((testWrapper "fib1.hk") 10)
                     , bench "12" $ whnfIO ((testWrapper "fib1.hk") 12)
                     , bench "14" $ whnfIO ((testWrapper "fib1.hk") 14)
                     , bench "16" $ whnfIO ((testWrapper "fib1.hk") 16)
                     , bench "18" $ whnfIO ((testWrapper "fib1.hk") 18)
                     , bench "20" $ whnfIO ((testWrapper "fib1.hk") 20)
                     , bench "22" $ whnfIO ((testWrapper "fib1.hk") 22)
                     , bench "24" $ whnfIO ((testWrapper "fib1.hk") 24)
                     , bench "24" $ whnfIO ((testWrapper "fib1.hk") 26)
                     , bench "24" $ whnfIO ((testWrapper "fib1.hk") 28)
                     ]
                ]


