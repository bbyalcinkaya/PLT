{-# LANGUAGE LambdaCase #-}

module Interpreter where

import CMM.Abs
import CMM.ErrM
import CMM.Print
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Environment
import Interpreter.Value
import Control.Exception.Base (throwIO, throw, runtimeError)

type Env = Environment Def (Maybe Val) -- Nothing for uninitialized variables

type Eval = StateT Env IO

type Exec = StateT Env (ExceptT Val IO)

-- an exception in Exec is not an error. it is a returned value

interpret :: Program -> IO ()
interpret p = do
  _ <- runExceptT $ runStateT (execProgram p) emptyEnvironment
  return ()

execProgram :: Program -> Exec ()
execProgram (PDefs p) = do
  mapM_ addFunction p
  exec (SExp $ EApp (Id "main") [])
  where
    -- add function definition to the environment
    addFunction :: Def -> Exec ()
    addFunction f@(DFun _ id _ _) = modify $ newFun id f

exec :: Stm -> Exec ()
exec = \case
  SExp e -> liftEval (eval e) >> return ()
  SDecls _ ids -> mapM_ (\id -> modify $ newVar id Nothing) ids
  SInit _ id exp -> do
    modify $ newVar id Nothing
    exp' <- liftEval $ eval exp
    modify $ fromErr . updateVar id (Just exp')
    return ()
  SReturn exp -> do
    e <- liftEval (eval exp)
    throwError e
  s@(SWhile cond body) -> do
    cond' <- liftEval $ eval cond
    if isTrue cond'
      then do
        exec $ SBlock [body]
        exec s
      else return ()
  SBlock stms -> do
    modify newBlock
    mapM_ exec stms
    modify dropBlock
  SIfElse cond tStm fStm -> do
    cond' <- liftEval $ eval cond
    if isTrue cond'
      then exec $ SBlock [tStm]
      else exec $ SBlock [fStm]

eval :: Exp -> Eval Val
eval = \case
  ECast typ exp -> cast typ <$> eval exp
  EBool LTrue -> return $ VBool True
  EBool LFalse -> return $ VBool False
  EInt i -> return . VInt $ fromIntegral i
  EDouble d -> return $ VDouble d
  EId id -> do
    v <- gets $ fromErr . lookupVar id
    case v of
      Nothing -> throw $ userError ("uninitialized variable " ++ show id)
      Just v -> return v
  EApp fId exps -> do
    vs <- mapM eval exps
    case Map.lookup fId builtIns of
      Just f -> liftIO $ f vs
      Nothing -> do
        DFun typ _ args stmts <- gets $ fromErr . lookupFun fId
        env <- get -- save the old env  
        put $ makeEnv env args vs
        r <- runExec $ mapM_ exec stmts -- exec the function body
        put env -- put the old env
        case r of
          Left v -> return v
          Right () ->
            if typ == Type_void
              then return VVoid
              else throw $ userError "Function did not return anything"
  EPost id op -> do
    -- x=5; printInt(x ++);
    v <- gets $ fromErr . lookupVar id
    case v of
      Just v' -> do
        modify $ fromErr . updateVar id (Just $ incDec op v')
        return v'
      Nothing -> throw $ userError $ "uninitialized variable " ++ show id
  EPre op id -> do
    v <- gets $ fromErr . lookupVar id
    case v of
      Just v' -> do
        let v'' = incDec op v'
        modify $ fromErr . updateVar id (Just v'')
        return v''
      Nothing -> throw $ userError $ "uninitialized variable " ++ show id
  EMul lexp op rexp -> evalBinaryOp (mul op) lexp rexp
  EAdd lexp op rexp -> evalBinaryOp (add op) lexp rexp
  EAss id exp -> do
    exp' <- eval exp
    modify $ fromErr . updateVar id (Just exp')
    --- catch
    return exp'
  ECmp lexp op rexp -> evalBinaryOp (comp op) lexp rexp
  -- && and || are lazy
  EAnd lexp rexp -> do
    lexp' <- eval lexp
    if isTrue lexp'
      then eval rexp
      else return lexp'
  EOr lexp rexp -> do
    lexp' <- eval lexp
    if isTrue lexp'
      then return lexp'
      else eval rexp

evalBinaryOp :: (Val -> Val -> Val) -> Exp -> Exp -> Eval Val
evalBinaryOp op l r = do
  vl <- eval l
  vr <- eval r
  return (op vl vr)

liftEval :: Eval a -> Exec a
liftEval ev = StateT (lift . runStateT ev)

runExec :: Exec a -> Eval (Either Val a)
runExec ex = StateT (\env -> runner env >>= wrapper env)
  where
    runner = runExceptT . runStateT ex
    wrapper env = \case
      Left val -> return (Left val, env)
      Right (a, env') -> return (Right a, env')

fromErr :: Err a -> a
fromErr (Bad err) = throw $ userError err
fromErr (Ok x) = x

makeEnv :: Env -> [Arg] -> [Val] -> Env
makeEnv oldEnv args vs = folded
  where
    zipped = zipWith zipper args vs
    zipper (ADecl _ id) v = (id, v)
    folded = foldl f (newBlock . emptyContext $ oldEnv) zipped
    f env (id, v) = newVar id (Just v) env