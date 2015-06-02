
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

analyzeTypesFunc :: Tree -> (String, Map.Map String (Set.Set Type))
analyzeTypesFunc (Func name args st) =    (name, analyzeTypes st (Map.fromList (zip args (repeat (Set.fromList [IntType, BoolType])))))

-- Takes a tree and a Map from variable name to a set of all possible
-- types the variable can take.
analyzeTypes :: Tree -> Map.Map String (Set.Set Type) -> Map.Map String (Set.Set Type)
analyzeTypes (Seq st1 st2) myMap =      let myMap' = analyzeTypes st1 myMap
                                        in  analyzeTypes st2 myMap'
analyzeTypes (Assign var expr) myMap =  let typSet = checkExpr expr myMap
                                        in  mergeTypeMap myMap (Map.singleton var typSet)
analyzeTypes (If expr st1 st2) myMap =  let myMap1 = analyzeTypes st1 myMap
                                            myMap2 = analyzeTypes st2 myMap
                                        in mergeTypeMap myMap1 myMap2
analyzeTypes (While expr st) myMap =    analyzeTypes st myMap
analyzeTypes (Skip) myMap =             myMap
analyzeTypes (Func name _ _) myMap =    error ("Function " ++ name ++ " should not be in the tree!")

mergeTypeMap :: Map.Map String (Set.Set Type) -> Map.Map String (Set.Set Type) -> Map.Map String (Set.Set Type)
mergeTypeMap map1 map2 = Map.unionWithKey (\k s1 s2 ->
                                case Set.null (Set.difference s1 s2) || Set.null (Set.intersection s1 s2) of
                                    True ->     Set.intersection s1 s2
                                    False ->    error ((show s1) ++ " does not match " ++ (show s2) ++ " for " ++ (show k))
                                ) map1 map2

checkEqual :: Show a => Bool -> Bool -> [a] -> Bool
checkEqual b1 b2 lst = case (b1 == b2) of
    True ->     True
    False ->    error (foldl (\acc x -> acc ++ x) "" ("Incorrect Types: ":(map show lst)))

checkExpr :: Expr -> Map.Map String (Set.Set Type) -> Set.Set Type
checkExpr expr env = case expr of
    Var str ->              env Map.! str
    Val val ->              case val of
                                IntValue i1 typ ->  Set.singleton typ
                                BoolValue b1 typ -> Set.singleton typ
    Negate expr' ->         checkExpr expr' env
    Comb op ex1 ex2 ->      let typ1 = checkExpr ex1 env
                                typ2 = checkExpr ex2 env
                            in  valComb op typ1 typ2
    -- FuncCall name args ->   let -- Convert Expr to Types
    --                             argTypes = map (\e -> checkExpr e env fEnv) args
    --                             -- Create local Map for function call. Add in arguments.
    --                             (argNames, stmt) = fEnv Map.! name
    --                             tempTypes = Map.fromList (zip argNames argTypes)
    --                             (bool1, tempTypes', _) = checkStmt stmt tempTypes fEnv
    --                         in  Maybe.fromJust (Map.lookup "output" tempTypes')
    --                   `          -- Throws type error when "output" is not assigned.
    --                             -- TODO: Is this intended behavior?

valComb :: OpComb -> Set.Set Type -> Set.Set Type -> Set.Set Type
valComb op s1 s2
    | op == EqualTo && not (Set.null (Set.intersection s1 s2)) = Set.intersection s1 s2
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

typeFile :: String -> IO (Bool, Types, FuncStorage)
typeFile file = do
    stmt <- parseFile file
    putStrLn "### Raw ASN: "
    print stmt
    putStrLn ""
    let (stmt', funcs) = sepFuncs stmt Map.empty
        listFuncs = Map.elems funcs
        (listFuncArgs, listFuncStmts) = unzip listFuncs
    putStrLn "### ASN after Function Separation is: "
    print stmt'
    putStrLn "### Functions: "
    print listFuncStmts
    putStrLn ""
    let globalTree = toTree stmt'
        listFuncTrees = map (\s -> toTree s) listFuncStmts
    putStrLn "### ASTs:"
    print globalTree
    print listFuncTrees
    putStrLn ""
    let listFuncArgTypes = map (\tr -> analyzeTypesFunc tr) listFuncTrees
    putStrLn "### Function arguments with possible types: "
    print listFuncArgTypes
    putStrLn ""
    -- let (b, types, funcSt) = checkStmt stmt' Map.empty Map.empty
    -- return (b, types, funcSt)
    return (True, Map.empty, Map.empty)
