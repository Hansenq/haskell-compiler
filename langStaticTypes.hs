
module LangStaticTypes where

import LangStaticDef
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Debug.Trace
import Data.Tuple

type Types = Map.Map String Type
type FuncStorage = Map.Map String ([String], Stmt)
type Tree = Stmt
type FuncStmts = Map.Map String ([(Type, String)], Tree)
type FuncInfo = Map.Map String ([String], Types)

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
sepFuncs (Func name args st) funcs =    (Skip, Map.insert name (args, (Func name args st)) funcs)
sepFuncs a b =                          (a, b)

-- Takes an Abstract Syntax Notation (with no function definitions) and
-- returns an AST.
toTree :: Stmt -> Tree
toTree (Seq (If expr st1 st2) endSt) =  (If expr (toTree (Seq st1 endSt)) (toTree (Seq st2 endSt)))
toTree (Seq st1 st2) =                  (Seq (toTree st1) (toTree st2))
toTree (If expr st1 st2) =              (If expr (toTree st1) (toTree st2))
toTree (While expr st) =                (While expr (toTree st))
toTree (Func name args st) =            (Func name args (toTree st))
toTree x =                              x

analyzeTypesFunc :: Tree -> FuncInfo -> (String, ([String], Types))
analyzeTypesFunc (Func name args st) myFuncs =
    (name, ((map snd args), analyzeTypes st (Map.fromList (map swap args)) myFuncs))

-- Takes a tree and a Map from variable name to a set of all possible
-- types the variable can take.
analyzeTypes :: Tree -> Types -> FuncInfo -> Types
analyzeTypes (Seq st1 st2) myMap myFuncs =      let myMap' = analyzeTypes st1 myMap myFuncs
                                                in  analyzeTypes st2 myMap' myFuncs
analyzeTypes (Declaration str expr typ) myMap myFuncs =
                                                let (myMap', typ') = checkExpr expr myMap myFuncs
                                                in  if typ == typ'
                                                        then mergeTypeMap myMap' (Map.singleton str typ)
                                                        else error ((show typ) ++ " does not match " ++ (show expr) ++ " for var " ++ (show str))
analyzeTypes (Assign var expr) myMap myFuncs =  let (myMap', typ) = checkExpr expr myMap myFuncs
                                                in  mergeTypeMap myMap' (Map.singleton var typ)
analyzeTypes (If expr st1 st2) myMap myFuncs =  let myMap1 = analyzeTypes st1 myMap myFuncs
                                                    myMap2 = analyzeTypes st2 myMap myFuncs
                                                in mergeTypeMap myMap1 myMap2
analyzeTypes (While expr st) myMap myFuncs =    analyzeTypes st myMap myFuncs
analyzeTypes (Skip) myMap myFuncs =             myMap
analyzeTypes (Func name _ _) myMap myFuncs =    error ("Function " ++ name ++ " should not be in the tree!")

mergeTypeMap :: Types -> Types -> Types
mergeTypeMap map1 map2 = Map.unionWithKey (\k s1 s2 -> case s1 == s2 of
                                    False ->    error ((show s1) ++ " does not match " ++ (show s2) ++ " for key " ++ (show k))
                                    True ->     s1 -- Bias to left
                                    ) map1 map2

checkExpr :: Expr -> Types -> FuncInfo -> (Types, Type)
checkExpr expr env funcs = case expr of
    Var str ->              (env, env Map.! str)
    Val val ->              case val of
                                IntValue i1 typ ->  (env, typ)
                                BoolValue b1 typ -> (env, typ)
    Negate expr' ->         checkExpr expr' env funcs
    Comb op ex1 ex2 ->      let (env', typ1) = checkExpr ex1 env funcs
                                (env'', typ2) = checkExpr ex2 env' funcs
                                resType = valComb op typ1 typ2
                            in  (env'', resType)
    FuncCall name args ->   case Map.lookup name funcs of -- TODO: funcs can be empty map
                                Just res ->
                                    let (funcArgNames, funcVarTypes) = res
                                        -- TODO: Map.! fails when an argument of a function doesn't have a type.
                                        funcArgReqTypes = map (\argName -> funcVarTypes Map.! argName) funcArgNames -- Should be [Type, Type,  ...]
                                        -- args is a list of expressions. This updates the list of types for each variable, then
                                        -- returns the list of types for each argument.
                                        env' = foldl (\acc x -> fst $ checkExpr x acc funcs) env args
                                        varTypes = map (\arg -> snd $ checkExpr arg env funcs) args
                                        -- If any arguments do not fit the type, the error should be thrown here.
                                        correct = foldl (\acc (prop, req) -> prop == req && acc) True (zip varTypes funcArgReqTypes)
                                        -- TODO: Need this if statement here because otherwise (lazy) Haskell won't evaluate mergedArgTypes.
                                        str = if correct then "output" else error "Arguments do not match declaration"
                                        outputType = funcVarTypes Map.! str -- TODO: what if there is none
                                    in  (env', outputType)
                                Nothing -> (env, UnknownType)


isVar :: Expr -> Bool
isVar (Var v) = True
isVar _ =       False

valComb :: OpComb -> Type -> Type -> Type
valComb op s1 s2
    | op == EqualTo && s1 == s2 = s1
    | op `elem` [GreaterThan, LessThan]
        && IntType == s1
        && IntType == s2 =
            BoolType
    | op `elem` [And, Or]
        && BoolType == s1
        && BoolType == s2 =
            BoolType
    | op `elem` [Add, Sub, Mult, Div]
        && IntType == s1
        && IntType == s2 =
            IntType
    | otherwise = error ((show s1) ++ " does not match " ++ (show s2) ++ " for operation " ++ (show op))

typeFile :: String -> IO (Bool, Types, FuncStorage)
typeFile file = do
    stmt <- parseFile file
    putStrLn "### Raw ASN: "
    print stmt
    putStrLn ""
    -- Separate out functions from the global statements
    let (stmt', funcs) = sepFuncs stmt Map.empty
        listFuncs = Map.elems funcs
        (listFuncArgs, listFuncStmts) = unzip listFuncs
    putStrLn "### ASN after Function Separation is: "
    print stmt'
    putStrLn "### Functions: "
    print listFuncStmts
    putStrLn ""
    -- Change the global statements and the function statements to ASTs
    let globalTree = toTree stmt'
        listFuncTrees = map (\s -> toTree s) listFuncStmts
    putStrLn "### ASTs:"
    print globalTree
    print listFuncTrees
    putStrLn ""
    -- Soft Typing: Obtain possible types for every variable in each function
    let listFuncArgTypes = map (\tr -> analyzeTypesFunc tr Map.empty) listFuncTrees
    putStrLn "### Function arguments with possible types: "
    print listFuncArgTypes -- [(name, ([args], Map varName (Set Type))), ...]
    putStrLn ""
    -- Get output type from function
    let funcTypes = Map.fromList listFuncArgTypes
        listFuncArgTypes2 = map (\tr -> analyzeTypesFunc tr funcTypes) listFuncTrees
    putStrLn "### Function arguments with possible types (2): "
    print listFuncArgTypes2 -- [(name, ([args], Map varName (Set Type))), ...]
    putStrLn ""
    -- Get output type from function
    let funcTypes = Map.fromList listFuncArgTypes2
        listFuncArgTypes3 = map (\tr -> analyzeTypesFunc tr funcTypes) listFuncTrees
    putStrLn "### Function arguments with possible types (3): "
    print listFuncArgTypes3 -- [(name, ([args], Map varName (Set Type))), ...]
    putStrLn ""
    -- TODO: How many times do we have to repeat this until all types are accurate?

    -- Analyze global statements down here (after we know everything about the functions)
    -- let (b, types, funcSt) = checkStmt stmt' Map.empty Map.empty
    -- return (b, types, funcSt)
    return (True, Map.empty, Map.empty)
