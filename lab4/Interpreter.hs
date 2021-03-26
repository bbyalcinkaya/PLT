{-# LANGUAGE LambdaCase #-}

-- | Interpreter for lambda-calculus with if, +, -, <.
--
--   Strategy can be either call-by-value or call-by-name.
module Interpreter (interpret, Strategy (..)) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Fun.Abs
import Fun.Print

-- | Evaluation strategy.
data Strategy
  = CallByName
  | CallByValue
  deriving (Show)

-- | Error monad.
type Err = Except String

-- | Entry point: Program computes a number.
interpret :: Strategy -> Program -> Err Integer
interpret strategy (Prog defs (DMain mainExp)) = do
  let mkLam xs e = foldr EAbs xs e
  let mkDef (DDef f xs e) = (f, mkLam e xs)
  let sig = Map.fromList $ map mkDef defs
  let cxt =
        Cxt
          { cxtStrategy = strategy,
            cxtSig = sig,
            cxtEnv = Map.empty
          }

  runReaderT (eval mainExp >>= intValue) cxt

data Cxt = Cxt
  { cxtStrategy :: Strategy,
    cxtSig :: Sig,
    cxtEnv :: Env
  }
  deriving (Show)

data Value
  = VInt Integer
  | VClos Ident Exp Env -- function closures
  | VVClos Exp Env -- value (?) closures
  deriving (Show)

type Sig = Map Ident Exp

type Env = Map Ident Entry

type Entry = Value

type Eval = ReaderT Cxt Err

intValue :: Value -> Eval Integer
intValue (VInt i) = return i
intValue _ = throwError "result is not an integer"

setEnv :: Env -> Cxt -> Cxt
setEnv env cxt = cxt {cxtEnv = env}

insideEnv :: Env -> Eval a -> Eval a
insideEnv env = withReaderT (setEnv env)

eval :: Exp -> Eval Value
eval = \case
  EInt i -> return $ VInt i
  EVar x ->
    asks (Map.lookup x . cxtEnv) >>= \case
      Just v -> eval' v
      Nothing ->
        asks (Map.lookup x . cxtSig) >>= \case
          Just e -> insideEnv Map.empty $ eval e
          Nothing -> throwError $ unwords ["unbound variable", printTree x]
  EAbs x e -> asks $ VClos x e . cxtEnv -- get the environment and create a closure
  EApp f a ->
    asks cxtStrategy >>= \case
      CallByValue -> do
        g <- eval f
        v <- eval a
        apply g v
      CallByName -> do
        g <- eval' =<< eval f
        case g of
          VClos x e closEnv -> do
            env <- asks cxtEnv
            let argClos = VVClos a env
            let closEnv' = Map.insert x argClos closEnv
            insideEnv closEnv' $ eval e
          VInt _ -> throwError "Integer value cannot be applied, expected function value"
          _ -> throwError "Call by name "
  EAdd e e' -> do
    iv <- intValue =<< eval e
    iv' <- intValue =<< eval e'
    return $ VInt (iv + iv')
  ESub e e' -> do
    iv <- intValue =<< eval e
    iv' <- intValue =<< eval e'
    return $ VInt (iv - iv')
  ELt e e' -> do
    v <- intValue =<< eval e
    v' <- intValue =<< eval e'
    return $ if v < v' then VInt 1 else VInt 0
  EIf c t e -> do
    ic' <- intValue =<< eval c
    if ic' == 1
      then eval t
      else eval e

-- | Evaluate VVClos values again
eval' :: Value -> Eval Value
eval' (VVClos e env) = insideEnv env $ eval e
eval' v = return v

apply :: Value -> Value -> Eval Value
apply f v =
  case f of
    VInt {} -> throwError "Integer value cannot be applied, expected function value"
    VClos x e env -> insideEnv (Map.insert x v env) $ eval e
    vvc@(VVClos _ _) -> do
      fv <- eval' vvc
      apply fv v
