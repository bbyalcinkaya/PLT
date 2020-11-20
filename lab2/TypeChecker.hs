module TypeChecker where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import CMM.Abs
import CMM.Print
import CMM.ErrM



typecheck :: Program -> Err Program
typecheck (PDefs funDefs) = do
    env <- createSignatures funDefs
    case lookupFun env (Id "main") of
        Bad _ -> Bad "no main function"
        Ok (params, retType) -> do
            case (params, retType) of
                ([], Type_int) -> do 
                    defs <- checkFunctions env funDefs
                    return (PDefs defs)
                _ -> Bad "incorrect type for main function"
type Env = (Signatures, [Context])
type Signatures = Map Id ([Type], Type)
type Context  = Map Id Type

createSignatures :: [Def] -> Err Env
createSignatures = foldM createSignature emptyEnv

builtIns :: Signatures
builtIns = Map.fromList [ (Id "printInt", ([Type_int], Type_void))
                        , (Id "printDouble", ([Type_double], Type_void))
                        , (Id "readInt", ([], Type_int))
                        , (Id "readDouble", ([], Type_double))
                        ]

createSignature :: Env -> Def -> Err Env
createSignature env (DFun retType id args _)
    = case lookupFun env id of
        Ok _ -> Bad "Duplicate definitions of function "
        Bad _ -> updateFun env id (map argType args, retType)
    where
        argType :: Arg -> Type
        argType (ADecl t _) = t

checkFunctions :: Env -> [Def] -> Err [Def]
checkFunctions signatures defs = do
    defs' <- mapM (checkFunction signatures) defs
    return defs'

checkFunction :: Env -> Def -> Err Def
checkFunction env (DFun retType id args stmts) = do
    env' <- foldM insertArg (newBlock env) args
    (_, typedStmts) <- checkStmts env' retType stmts
    return (DFun retType id args typedStmts)

checkStmts :: Env -> Type -> [Stm] -> Err (Env, [Stm])
checkStmts env retType [] = return (env, [])
checkStmts env retType (x:xs) = do
    (env', x') <- checkStmt env retType x
    (env'', xs') <- checkStmts env' retType xs
    return (env'', x' : xs')

checkStmt :: Env -> Type -> Stm -> Err (Env, Stm)
checkStmt env _ (SExp exp) = do
    exp' <- inferExp env exp
    return (env, SExp exp')
checkStmt env _ stm@(SDecls typ identifiers) = do
    if typ == Type_void
        then Bad "cannot declare void variable"
        else do 
            env' <- foldM (\e i -> updateVar e i typ) env identifiers
            return (env', stm)
checkStmt env _ (SInit typ id exp) = do
    env' <- updateVar env id typ
    exp' <- checkExp env' typ exp
    return (env', SInit typ id exp')
checkStmt env retType (SReturn exp) = do
    exp' <- checkExp env retType exp
    return (env, SReturn exp')
checkStmt env retType (SWhile exp body) = do
    exp' <- checkExp env Type_bool exp
    (_, body') <- checkStmt (newBlock env) retType body
    return (env, SWhile exp' body')
checkStmt env retType (SBlock smts) = do
    (env', stmts') <- checkStmts (newBlock env) retType smts
    return (env, SBlock stmts')
checkStmt env retType (SIfElse exp tStmt fStmt) = do
    exp' <- checkExp env Type_bool exp
    (_, tStmt') <- checkStmt (newBlock env) retType tStmt
    (_ , fStmt') <- checkStmt (newBlock env) retType fStmt
    return (env, SIfElse exp' tStmt' fStmt')

checkExp :: Env -> Type -> Exp -> Err Exp
checkExp env typ exp = do
    exp' <- inferExp env exp
    case exp' of
        ECast typ2 _ -> if typ2 == typ then
                            return exp'
                        else
                            Bad $ "type of " ++ printTree exp ++
                                "expected " ++ printTree typ ++
                                "but found " ++ printTree typ2
        _ -> Bad "unexpected error"

inferExp :: Env -> Exp -> Err Exp
inferExp _ e@(EBool _) = Ok $ ECast Type_bool e -- Type_bool
inferExp _ e@(EInt _) = Ok $ ECast Type_int e
inferExp _ e@(EDouble _) = Ok $ ECast Type_double e
inferExp env e@(EId id) = do
    typ <- lookupVar env id
    return $ ECast typ e
inferExp env e@(EApp id exps) = do
    (argTypes, retType) <- lookupFun env id
    if length argTypes /= length exps
        then Bad "missing argument" -- TODO better error message
        else do
            zipWithM_ (checkExp env) argTypes exps 
            return $ ECast retType e -- TODO 
            
inferExp env e@(EPost id operator) = do
    typ <- lookupVar env id
    if typ == Type_double || typ == Type_int
        then return $ ECast typ e
        else Bad $ "Cannot increment/decrement " -- TODO msg
inferExp env e@(EPre operator id) = do 
    typ <- lookupVar env id
    if typ == Type_double || typ == Type_int
        then return $ ECast typ e
        else Bad $ "Cannot increment/decrement " -- TODO msg
inferExp env (EMul lexp mulOp rexp) = do

    el' <- inferExp env lexp
    case el' of
        ECast ltype el -> do
            er' <- inferExp env rexp
            case er' of
                ECast rtype er -> do
                    mType <- maxType ltype rtype
                    if equivalentArithmetic ltype rtype
                        then return $ ECast mType (EMul (ECast mType lexp) mulOp (ECast mType rexp))
                        else Bad "operands of mul must be of the same type Mul"
                _ -> Bad "impossible"
        _ -> Bad "impossible"
inferExp env (EAdd lexp addOp rexp) = do
    el' <- inferExp env lexp
    case el' of
        ECast ltype el -> do
            er' <- inferExp env rexp
            case er' of
                ECast rtype er -> do
                    mType <- maxType ltype rtype
                    if equivalentArithmetic ltype rtype
                        then return $ ECast mType (EAdd (ECast mType lexp) addOp (ECast mType rexp))
                        else Bad $ "operands of add must be of the same type " ++ show ltype ++ " " ++ show rtype
                _ -> Bad "impossible"
        _ -> Bad "impossible"
inferExp env e@(ECmp lexp cmpOp rexp)
    | cmpOp == ONEq || cmpOp == OEq = do
        el' <- inferExp env lexp
        case el' of
            ECast ltype el -> do
                er' <- inferExp env rexp
                case er' of
                    ECast rtype er -> do
                        mType <- maxType ltype rtype
                        if mType `elem` [Type_bool, Type_double, Type_int] then
                            if equivalent ltype rtype 
                                then return $ ECast Type_bool (ECmp (ECast mType lexp) cmpOp (ECast mType rexp)) 
                                else Bad "operands of comparison must be of the same type"
                        else
                            Bad "operands of comparison must be bool, int, or double"
                    _ -> Bad "impossible"
            _ -> Bad "impossible"
    | otherwise = do
        el' <- inferExp env lexp
        case el' of
            ECast ltype el -> do
                er' <- inferExp env rexp
                case er' of
                    ECast rtype er -> do
                        mType <- maxType ltype rtype
                        if equivalentArithmetic ltype rtype
                            then return $ ECast Type_bool (ECmp (ECast mType lexp) cmpOp (ECast mType rexp))
                            else Bad "operands of comparison must be of the same type"
                    _ -> Bad "impossible"
            _ -> Bad "impossible"
inferExp env e@(EAnd lexp rexp) = do
    checkExp env Type_bool lexp
    checkExp env Type_bool rexp
    return $ ECast Type_bool e
inferExp env e@(EOr lexp rexp) = do
    checkExp env Type_bool lexp
    checkExp env Type_bool rexp
    return $ ECast Type_bool e
inferExp env (EAss id exp) = do
    typ <- lookupVar env id
    exp' <- checkExp env typ exp
    return $ ECast typ exp'

equivalentArithmetic :: Type -> Type -> Bool
equivalentArithmetic Type_double Type_int = True
equivalentArithmetic Type_int Type_double = True
equivalentArithmetic Type_int Type_int = True
equivalentArithmetic Type_double Type_double = True
equivalentArithmetic _ _ = False

equivalent :: Type -> Type -> Bool
equivalent Type_double Type_int = True
equivalent Type_int Type_double = True
equivalent a b = a == b

maxType :: Type -> Type -> Err Type
maxType Type_double Type_int = Ok Type_double
maxType Type_int Type_double = Ok Type_double
maxType a b = if equivalent a b then Ok a else Bad "types are not equivalent"

isSubType :: Type -> Type -> Bool
isSubType Type_int Type_double = True
isSubType a b =  a == b

insertArg :: Env -> Arg -> Err Env
insertArg env (ADecl t i) = updateVar env i t

-- environment functions 

lookupVar :: Env -> Id -> Err Type
lookupVar (_, []) (Id var) = Bad $ "Variable not initialized: " ++ var
lookupVar (sig, context:rest) id = case Map.lookup id context of
    Just x -> Ok x
    Nothing -> lookupVar (sig, rest) id

lookupFun :: Env -> Id -> Err ([Type],Type)
lookupFun (sig, _) id@(Id str) = case Map.lookup id sig of
    Just x -> Ok x
    Nothing -> Bad $ "Function not found: " ++ str

updateVar :: Env -> Id -> Type -> Err Env
updateVar (sig, context:rest) id@(Id var) varType = case Map.lookup id context of
    Just _ -> Bad $ "Duplicate declarations of " ++ var
    Nothing -> Ok (sig, Map.insert id varType context : rest)

updateFun :: Env -> Id -> ([Type],Type) -> Err Env
updateFun (sig, context) id (parameters, funType) = do
    checkParameters parameters
    Ok (Map.insert id (parameters, funType) sig, context)

checkParameters :: [Type] -> Err ()
checkParameters [] = return ()
checkParameters (parameter: rest) = case parameter of
    Type_void -> Bad "Cannot have void types for parameters"
    _ -> checkParameters rest

newBlock  :: Env -> Env
newBlock (env, context) = (env, Map.empty : context)

emptyEnv  :: Env
emptyEnv = (builtIns, [])

-- MonadFail
