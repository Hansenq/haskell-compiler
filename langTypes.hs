
module LangTypes where

import LangDef
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Debug.Trace

type Types = Map.Map String Type
type FuncStorage = Map.Map String ([String], Stmt)
type Tree = Stmt
type FuncStmts = Map.Map String ([String], Tree)
type PossTypes = Map.Map String (Set.Set Type)
type FuncInfo = Map.Map String ([String], PossTypes)

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

analyzeTypesFunc :: Tree -> FuncInfo -> (String, ([String], PossTypes))
analyzeTypesFunc (Func name args st) myFuncs =
    (name, (args, analyzeTypes st (Map.fromList (zip args (repeat (Set.fromList [IntType, BoolType])))) myFuncs))

-- Takes a tree and a Map from variable name to a set of all possible
-- types the variable can take.
analyzeTypes :: Tree -> PossTypes -> FuncInfo -> PossTypes
analyzeTypes (Seq st1 st2) myMap myFuncs =      let myMap' = analyzeTypes st1 myMap myFuncs
                                                in  analyzeTypes st2 myMap' myFuncs
analyzeTypes (Assign var expr) myMap myFuncs =  let (myMap', typSet) = checkExpr expr myMap myFuncs
                                                in  mergeTypeMap myMap' (Map.singleton var typSet)
analyzeTypes (If expr st1 st2) myMap myFuncs =  let myMap1 = analyzeTypes st1 myMap myFuncs
                                                    myMap2 = analyzeTypes st2 myMap myFuncs
                                                in mergeTypeMap myMap1 myMap2
analyzeTypes (While expr st) myMap myFuncs =    analyzeTypes st myMap myFuncs
analyzeTypes (Skip) myMap myFuncs =             myMap
analyzeTypes (Func name _ _) myMap myFuncs =    error ("Function " ++ name ++ " should not be in the tree!")

mergeTypeMap :: PossTypes -> PossTypes -> PossTypes
mergeTypeMap map1 map2 = Map.unionWith (\s1 s2 -> mergeTypeSet s1 s2) map1 map2

mergeTypeSet :: Set.Set Type -> Set.Set Type -> Set.Set Type
mergeTypeSet set1 set2 = case Set.null (Set.intersection set1 set2) of
                                    False ->    Set.intersection set1 set2
                                    True ->     error ((show set1) ++ " does not match " ++ (show set2))

checkEqual :: Show a => Bool -> Bool -> [a] -> Bool
checkEqual b1 b2 lst = case (b1 == b2) of
    True ->     True
    False ->    error (foldl (\acc x -> acc ++ x) "" ("Incorrect Types: ":(map show lst)))

checkExpr :: Expr -> PossTypes -> FuncInfo -> (PossTypes, Set.Set Type)
checkExpr expr env funcs = case expr of
    Var str ->              (env, env Map.! str)
    Val val ->              case val of
                                IntValue i1 typ ->  (env, Set.singleton typ)
                                BoolValue b1 typ -> (env, Set.singleton typ)
    Negate expr' ->         checkExpr expr' env funcs
    Comb op ex1 ex2 ->      let (env', typ1) = checkExpr ex1 env funcs
                                (env'', typ2) = checkExpr ex2 env' funcs
                                env''' = equalizeTermTypes op (ex1,typ1) (ex2,typ2) env'' funcs
                                resType = valComb op typ1 typ2
                            in  (env''', resType)
    FuncCall name args ->   case Map.lookup name funcs of -- TODO: funcs can be empty map
                                Just res ->
                                    let (funcArgNames, funcVarTypes) = res
                                        -- TODO: Map.! fails when an argument of a function doesn't have a type.
                                        funcArgReqTypes = map (\argName -> funcVarTypes Map.! argName) funcArgNames -- Should be [Set, Set, ...]
                                        -- args is a list of expressions. This updates the list of types for each variable, then
                                        -- returns the list of types for each argument.
                                        env' = foldl (\acc x -> fst $ checkExpr x acc funcs) env args
                                        varTypes = map (\arg -> snd $ checkExpr arg env funcs) args
                                        -- If any arguments do not fit the type, the error should be thrown here.
                                        mergedArgTypes = map (\(var, arg) -> mergeTypeSet var arg) (zip varTypes funcArgReqTypes)
                                        -- TODO: Need this if statement here because otherwise (lazy) Haskell won't evaluate mergedArgTypes.
                                        str = if (foldl (\acc x -> acc || Set.null x) False mergedArgTypes) == True then error "Arguments do not match declaration" else "output"
                                        outputType = funcVarTypes Map.! str -- TODO: what if there is none
                                    in  (env', outputType)
                                Nothing -> (env, Set.fromList [IntType, BoolType])

-- Takes a Comb operation, for variables involved in the Comb, equalizes both sides so
-- that they both must have the same type.
equalizeTermTypes :: OpComb -> (Expr, Set.Set Type) -> (Expr, Set.Set Type) -> PossTypes -> FuncInfo -> PossTypes
equalizeTermTypes op (ex1, typ1) (ex2, typ2) env funcs
    | op == EqualTo && (isVar ex1 || isVar ex2) =
        let intersect = Set.intersection typ1 typ2
            -- Hack
            intersect2 = if Set.null intersect then error "Types in the expression don't match!" else intersect
        in  case (ex1, ex2) of
                (Var v1, Var v2) ->     Map.insert v1 intersect2 (Map.insert v2 intersect2 env)
                (Var v1, _) ->          Map.insert v1 intersect2 env
                (_, Var v2) ->          Map.insert v2 intersect2 env
    | op `elem` [GreaterThan, LessThan, Add, Sub, Mult, Div]
        && (isVar ex1 || isVar ex2) =
        let resType = Set.singleton IntType
            intersect = if Set.null (Set.intersection typ1 typ2) then error "Types in the expression don't match!" else resType
        in  case (ex1, ex2) of
                (Var v1, Var v2) ->     Map.insert v1 intersect (Map.insert v2 intersect env)
                (Var v1, _) ->          Map.insert v1 intersect env
                (_, Var v2) ->          Map.insert v2 intersect env
    | op `elem` [And, Or]
        && (isVar ex1 || isVar ex2) =
        let resType = Set.singleton BoolType
            intersect = if Set.null (Set.intersection typ1 typ2) then error "Types in the expression don't match!" else resType
        in  case (ex1, ex2) of
                (Var v1, Var v2) ->     Map.insert v1 intersect (Map.insert v2 intersect env)
                (Var v1, _) ->          Map.insert v1 intersect env
                (_, Var v2) ->          Map.insert v2 intersect env
    | otherwise = env

isVar :: Expr -> Bool
isVar (Var v) = True
isVar _ =       False

valComb :: OpComb -> Set.Set Type -> Set.Set Type -> Set.Set Type
valComb op s1 s2
    | op == EqualTo && not (Set.null (Set.intersection s1 s2)) = Set.singleton BoolType
    | op `elem` [GreaterThan, LessThan]
        && Set.member IntType s1
        && Set.member IntType s2 =
            Set.singleton BoolType
    | op `elem` [And, Or]
        && Set.member BoolType s1
        && Set.member BoolType s2 =
            Set.singleton BoolType
    | op `elem` [Add, Sub, Mult, Div]
        && Set.member IntType s1
        && Set.member IntType s2 =
            Set.singleton IntType
    | otherwise = error ((show s1) ++ " does not match " ++ (show s2) ++ " for operation " ++ (show op))

showFuncSoftTypes :: [(String, ([String], PossTypes))] -> String
showFuncSoftTypes lst = foldl   (\acc (name, (args, fMap)) ->
                                    (acc ++ "Function \"" ++ name ++ "\": \n"
                                        ++ "Args: " ++ (show args) ++ "\n"
                                        ++ (foldl (\acc (k, set) -> acc ++ "\t" ++ k ++ ": " ++ (show set) ++ "\n") "" (Map.assocs fMap))
                                        )
                                ) "" lst

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
    putStrLn $ showFuncSoftTypes listFuncArgTypes -- [(name, ([args], Map varName (Set Type))), ...]
    putStrLn ""
    -- Get output type from function
    let funcTypes = Map.fromList listFuncArgTypes
        listFuncArgTypes2 = map (\tr -> analyzeTypesFunc tr funcTypes) listFuncTrees
    putStrLn "### Function arguments with possible types (2): "
    putStrLn $ showFuncSoftTypes listFuncArgTypes2 -- [(name, ([args], Map varName (Set Type))), ...]
    putStrLn ""
    -- Get output type from function
    let funcTypes = Map.fromList listFuncArgTypes2
        listFuncArgTypes3 = map (\tr -> analyzeTypesFunc tr funcTypes) listFuncTrees
    putStrLn "### Function arguments with possible types (3): "
    putStrLn $ showFuncSoftTypes listFuncArgTypes3 -- [(name, ([args], Map varName (Set Type))), ...]
    putStrLn ""
    -- TODO: How many times do we have to repeat this until all types are accurate?

    -- Analyze global statements down here (after we know everything about the functions)
    -- let (b, types, funcSt) = checkStmt stmt' Map.empty Map.empty
    -- return (b, types, funcSt)
    return (True, Map.empty, Map.empty)
