
module LangTypes where

import LangDef
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

type Types = Map.Map String Type
type FuncStorage = Map.Map String ([String], Stmt)

checkStmt :: Stmt -> Types -> FuncStorage -> (Bool, Types, FuncStorage)
checkStmt s env fEnv = case s of
    Seq st1 st2 ->          let (bool1, env', fEnv') = checkStmt st1 env fEnv
                                (bool2, env'', fEnv'') = checkStmt st2 env' fEnv'
                            in  checkEqual bool1 bool2 [st1,st2] (env'', fEnv'')
    Assign x expr ->        let typ = checkExpr expr env fEnv
                                existing = Map.lookup x env
                            in  case existing of
                                Nothing ->  (True, Map.insert x typ env, fEnv)
                                Just a ->   checkEqual (typ == a) True [s] (env, fEnv)
    If expr st1 st2 ->      let bool1 = (checkExpr expr env fEnv) == BoolType
                                (bool2, env', fEnv') = checkStmt st1 env fEnv
                                (b1, _, _) = checkEqual bool1 bool2 [expr] (env', fEnv')
                                (bool3, env'', fEnv'') = checkStmt st2 env' fEnv'
                            in  checkEqual b1 bool3 [expr] (env'', fEnv'')
                                -- TODO: Does this correctly take care of forks? Use Theoretical Model.
    While expr st ->        let bool1 = (checkExpr expr env fEnv) == BoolType
                                (bool2, env', fEnv') = checkStmt st env fEnv
                            in  checkEqual bool1 bool2 [expr] (env', fEnv')
    Skip  ->                (True, env, fEnv)
    Func name args st ->    (True, env, Map.insert name (args, st) fEnv)
                            -- TODO: Should type-check here?
    _ ->                    error "Statement not found."

checkEqual :: Show a => Bool -> Bool -> [a] -> (Types, FuncStorage) -> (Bool, Types, FuncStorage)
checkEqual b1 b2 lst (types, fEnv) = case (b1 == b2) of
    True ->     (True, types, fEnv)
    False ->    error ("Incorrect Types: ")

checkExpr :: Expr -> Types -> FuncStorage -> Type
checkExpr expr env fEnv = case expr of
    Var str ->              env Map.! str
    Val val ->              case val of
                                IntValue i1 typ ->  typ
                                BoolValue b1 typ -> typ
    Negate expr' ->         checkExpr expr' env fEnv
    Comb op ex1 ex2 ->      let typ1 = checkExpr ex1 env fEnv
                                typ2 = checkExpr ex2 env fEnv
                            in  valComb op typ1 typ2
    FuncCall name args ->   let -- Convert Expr to Types
                                argTypes = map (\e -> checkExpr e env fEnv) args
                                -- Create local Map for function call. Add in arguments.
                                (argNames, stmt) = fEnv Map.! name
                                tempTypes = Map.fromList (zip argNames argTypes)
                                (bool1, tempTypes', _) = checkStmt stmt tempTypes fEnv
                            in  Maybe.fromJust (Map.lookup "output" tempTypes')
                                -- Throws type error when "output" is not assigned.
                                -- TODO: Is this intended behavior?

valComb :: OpComb -> Type -> Type -> Type
valComb EqualTo _ _ = BoolType
valComb GreaterThan IntType IntType = BoolType
valComb LessThan IntType IntType = BoolType
valComb And BoolType BoolType = BoolType
valComb Or BoolType BoolType = BoolType
valComb Add IntType IntType = IntType
valComb Sub IntType IntType = IntType
valComb Mult IntType IntType = IntType
valComb Div IntType IntType = IntType
valComb op v1 v2 = error ("Incorrect Types: " ++ (show op) ++ " " ++ (show v1) ++ " " ++ (show v2))

newBool :: Bool -> Value
newBool bool = BoolValue bool BoolType

newInt :: Integer -> Value
newInt int = IntValue int IntType

evalFile :: String -> IO (Bool, Types, FuncStorage)
evalFile file = do
    stmt <- parseFile file
    print stmt
    let (val, env, fEnv) = checkStmt stmt Map.empty Map.empty
    putStrLn $ "Output is: " ++ (show $ env Map.! "output")
    return (val, env, fEnv)
