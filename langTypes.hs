
module LangTypes where

import LangDef
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

type Types = Map.Map String Type
type FuncStorage = Map.Map String ([String], Stmt)
type FuncStmts = Map.Map String Stmt

-- Takes an ASN and separates out the function definitions.
sepFuncs :: Stmt -> FuncStmts -> (Stmt, FuncStmts)
sepFuncs (Seq st1 st2) funcs =          let (st1', funcs') = sepFuncs st1 funcs
                                            (st2', funcs'') = sepFuncs st2 funcs'
                                        in  ((Seq st1' st2'), funcs'')
sepFuncs (If expr st1 st2) funcs =      let (st1', funcs') = sepFuncs st1 funcs
                                            (st2', funcs'') = sepFuncs st2 funcs'
                                        in  ((If expr st1' st2'), funcs'')
sepFuncs (While expr st) funcs =        let (st', funcs') = sepFuncs st funcs
                                        in  ((While expr st'), funcs')
sepFuncs (Func name args st) funcs =    (Skip, Map.insert name (Func name args st) funcs)
sepFuncs a b =                          (a, b)

-- Takes an Abstract Syntax Notation (with no function definitions) and
-- returns an AST.
toTree :: Stmt -> Stmt
toTree (Seq (If expr st1 st2) endSt) =  (If expr (toTree (Seq st1 endSt)) (toTree (Seq st2 endSt)))
toTree (Seq st1 st2) =                  (Seq (toTree st1) (toTree st2))
toTree (If expr st1 st2) =              (If expr (toTree st1) (toTree st2))
toTree (While expr st) =                (While expr (toTree st))
toTree (Func name args st) =            (Func name args (toTree st))
toTree x =                              x

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
                            -- TODO: This statement currently doesn't work because the AST is not a tree
                            in  checkEqual b1 bool3 [expr] (env'', fEnv'')
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

typeFile :: String -> IO (Bool, Types, FuncStorage)
typeFile file = do
    stmt <- parseFile file
    putStrLn "### Raw ASN: "
    print stmt
    putStrLn ""
    let (stmt', funcs) = sepFuncs stmt Map.empty
        listFuncs = Map.elems funcs
    putStrLn "### ASN after Function Separation is: "
    print stmt'
    putStrLn "### Functions: "
    print listFuncs
    putStrLn ""
    let asns = stmt':listFuncs
        trees = map (\s -> toTree s) asns
    putStrLn "### ASTs:"
    print trees
    let (b, types, funcSt) = checkStmt stmt' Map.empty Map.empty
    return (b, types, funcSt)
