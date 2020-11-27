{-# LANGUAGE LambdaCase #-}

module Interpreter where

import CMM.Abs
import CMM.ErrM
import CMM.Print
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Environment
import Interpreter.Value

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
  SDecls typ ids -> undefined
  SInit typ id exp -> undefined
  SReturn exp -> do
    e <- liftEval (eval exp)
    throwError e
  SWhile cond body -> undefined
  SBlock stms -> do
    modify newBlock
    mapM_ exec stms
    modify dropBlock
  SIfElse cond tStm fStm -> undefined

eval :: Exp -> Eval Val
eval = \case
  ECast typ exp -> cast typ <$> eval exp
  EBool LTrue -> return $ VBool True
  EBool LFalse -> return $ VBool False
  EInt i -> return . VInt $ fromIntegral i
  EDouble d -> return $ VDouble d
  EId id -> undefined
  EApp fId args -> undefined
  EPost id op -> undefined
  EPre op id -> undefined
  EMul lexp op rexp -> evalBinaryOp (mul op) lexp rexp
  EAdd lexp op rexp -> evalBinaryOp (add op) lexp rexp
  EAss id exp -> undefined
  ECmp lexp op rexp -> evalBinaryOp (comp op) lexp rexp
  -- && and || are lazy
  EAnd lexp rexp -> undefined
  EOr lexp rexp -> undefined

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