{-# LANGUAGE LambdaCase #-}

module TypeChecker where

import CMM.Abs
import CMM.ErrM
import CMM.Print
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.State
import Environment

type FunType = ([Type], Type)

type Env = Environment FunType Type

type Check = StateT Env Err

typecheck :: Program -> Err Program
typecheck p = evalStateT (checkProgram p) emptyEnvironment

checkProgram :: Program -> Check Program
checkProgram (PDefs funDefs) = do
  createSignatures funDefs
  mainType <- gets $ lookupFun (Id "main")
  case mainType of
    Ok ([], Type_int) -> return ()
    Ok _ -> throwError "unexpected type of main function"
    Bad _ -> throwError "no main function"
  PDefs <$> mapM checkFunction funDefs

createSignatures :: [Def] -> Check ()
createSignatures defs = do
  put emptyEnvironment
  mapM_ (modify . uncurry newFun) builtIns
  mapM_ createSignature defs
  where
    builtIns :: [(Id, FunType)]
    builtIns =
      [ (Id "printInt", ([Type_int], Type_void)),
        (Id "printDouble", ([Type_double], Type_void)),
        (Id "readInt", ([], Type_int)),
        (Id "readDouble", ([], Type_double))
      ]

createSignature :: Def -> Check ()
createSignature (DFun retType id@(Id str) args _) = do
  x <- gets $ lookupFun id
  when (isOk x) (throwError $ "duplicate definition of function " ++ str)
  when (Type_void `elem` argTypes) (throwError "cannot have void parameters")
  modify $ newFun id (argTypes, retType)
  where
    argTypes = map (\(ADecl t _) -> t) args
    isOk (Ok _) = True
    isOk _ = False

checkFunction :: Def -> Check Def
checkFunction (DFun retType id args body) = do
  modify newBlock
  mapM_ insertArg args
  body' <- mapM (checkStm retType) body
  modify dropBlock
  return $ DFun retType id args body'
  where
    insertArg :: Arg -> Check ()
    insertArg (ADecl t i) = declareVar t i

checkStm :: Type -> Stm -> Check Stm
checkStm retType = \case
  SExp exp -> SExp <$> inferExp exp
  stm@(SDecls typ ids) -> do
    when (typ == Type_void) (throwError "cannot declare void variable")
    mapM_ (declareVar typ) ids
    return stm
  SInit typ id exp -> do
    declareVar typ id
    exp' <- checkExp typ exp
    return $ SInit typ id exp'
  SReturn exp -> SReturn <$> checkExp retType exp
  SWhile exp body -> do
    exp' <- checkExp Type_bool exp
    body' <- withBlock (checkStm retType body)
    return $ SWhile exp' body'
  SBlock stms -> do
    stms' <- withBlock $ mapM (checkStm retType) stms
    return $ SBlock stms'
  SIfElse exp tStm fStm -> do
    exp' <- checkExp Type_bool exp
    tStm' <- withBlock $ checkStm retType tStm
    fStm' <- withBlock $ checkStm retType fStm
    return $ SIfElse exp' tStm' fStm'
  where
    withBlock :: Check a -> Check a
    withBlock c = do
      modify newBlock
      a <- c
      modify dropBlock
      return a
declareVar :: Type -> Id -> Check ()
declareVar typ id@(Id var) = do
  existing <- gets $ lookupScopeVar id
  case existing of
    Ok _ -> throwError $ "duplicate declaration of " ++ var
    Bad _ -> modify $ newVar id typ
    
checkExp :: Type -> Exp -> Check Exp
checkExp typ exp =
  inferExp exp >>= \case
    ECast typ2 exp' ->
      if typ2 `isSubType` typ -- handle coercions
        then return $ ECast typ exp' -- cast to expected type
        else
          throwError $
            "type of " ++ printTree exp ++ "\n"
              ++ "expected "
              ++ printTree typ
              ++ "\n"
              ++ "but found "
              ++ printTree typ2

inferExp :: Exp -> Check Exp
inferExp = \case
  e@(EBool _) -> return $ ECast Type_bool e
  e@(EDouble _) -> return $ ECast Type_double e
  e@(EInt _) -> return $ ECast Type_int e
  e@(EId id) -> do
    typ <- fromErr . gets $ lookupVar id
    return $ ECast typ e
  EApp id exps -> do
    (argTypes, retType) <- fromErr . gets $ lookupFun id
    when (length argTypes /= length exps) (throwError "missing/unexpected argument")
    exps' <- zipWithM checkExp argTypes exps
    return $ ECast retType (EApp id exps')
  e@(EPost id _) -> do
    typ <- fromErr . gets $ lookupVar id
    unless (isNumeric typ) (throwError $ "cannot increment/decrement" ++ show typ)
    return $ ECast typ e
  e@(EPre _ id) -> do
    typ <- fromErr . gets $ lookupVar id
    unless (isNumeric typ) (throwError $ "cannot increment/decrement" ++ show typ)
    return $ ECast typ e
  EMul lexp mulOp rexp -> inferArithmetic make lexp rexp
    where
      make l r = EMul l mulOp r
  EAdd lexp addOp rexp -> inferArithmetic make lexp rexp
    where
      make l r = EAdd l addOp r
  ECmp lexp cmpOp rexp -> inferCmp cmpOp lexp rexp
  e@(EAnd lexp rexp) -> do
    checkExp Type_bool lexp
    checkExp Type_bool rexp
    return $ ECast Type_bool e
  e@(EOr lexp rexp) -> do
    checkExp Type_bool lexp
    checkExp Type_bool rexp
    return $ ECast Type_bool e
  EAss id exp -> do
    typ <- fromErr . gets $ lookupVar id
    exp' <- checkExp typ exp
    return $ ECast typ (EAss id exp')

fromErr :: Check (Err a) -> Check a
fromErr c = do
  a <- c
  case a of
    Bad err -> throwError err
    Ok x -> return x

inferBinArg :: Exp -> Exp -> Check (Type, Exp, Exp)
inferBinArg lexp rexp =
  -- infer lhs
  inferExp lexp >>= \case
    -- infer rhs
    ECast ltype el ->
      inferExp rexp >>= \case
        ECast rtype er -> case maxType ltype rtype of
          Bad err -> throwError err
          Ok mType -> return (mType, ECast mType el, ECast mType er)
        -- result of inferExp is always an ECast
        _ -> throwError "impossible"
    _ -> throwError "impossible"

inferArithmetic :: (Exp -> Exp -> Exp) -> Exp -> Exp -> Check Exp
inferArithmetic make lexp rexp = do
  (typ, lexp', rexp') <- inferBinArg lexp rexp
  unless (isNumeric typ) (throwError "invalid argument type in arithmetic expression")
  return $ ECast typ (make lexp' rexp')

inferCmp :: CmpOp -> Exp -> Exp -> Check Exp
inferCmp cmpOp lexp rexp = do
  (typ, lexp', rexp') <- inferBinArg lexp rexp
  unless (isComparable cmpOp typ) (throwError "invalid argument type in comparison")
  return $ ECast Type_bool (ECmp lexp' cmpOp rexp')

isNumeric :: Type -> Bool
isNumeric Type_double = True
isNumeric Type_int = True
isNumeric _ = False

isComparable :: CmpOp -> Type -> Bool
isComparable cmpOp t
  | cmpOp `elem` [OEq, ONEq] = Type_void /= t
  | otherwise = t `elem` [Type_double, Type_int]

maxType :: Type -> Type -> Err Type
maxType Type_double Type_int = Ok Type_double
maxType Type_int Type_double = Ok Type_double
maxType a b = if a == b then Ok a else Bad "types are not equivalent"

isSubType :: Type -> Type -> Bool
isSubType Type_int Type_double = True
isSubType a b = a == b
