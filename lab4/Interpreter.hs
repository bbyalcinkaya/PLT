{-# LANGUAGE LambdaCase #-}

-- | Interpreter for lambda-calculus with if, +, -, <.
--
--   Strategy can be either call-by-value or call-by-name.
module Interpreter (interpret, Strategy (..)) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor
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

  v <- eval cxt mainExp

  intValue v

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

intValue :: Value -> Err Integer
intValue (VInt i) = return i
intValue _ = throwError "result is not an integer"

type Sig = Map Ident Exp

type Env = Map Ident Entry

type Entry = Value

eval :: Cxt -> Exp -> Err Value
eval cxt = \case
  EInt i -> return $ VInt i
  EVar x -> do
    case Map.lookup x $ cxtEnv cxt of
      Just v -> eval' cxt v
      Nothing -> case Map.lookup x $ cxtSig cxt of
        Just e -> eval cxt{ cxtEnv = Map.empty } e
        Nothing -> throwError $ unwords ["unbound variable", printTree x]
  EAbs x e -> return $ VClos x e (cxtEnv cxt)
  EApp f a -> case cxtStrategy cxt of
    CallByValue -> do
      g <- eval cxt f
      v <- eval cxt a
      apply cxt g v
    CallByName -> do
      g <- eval' cxt =<< eval cxt f
      case g of
        (VClos x e env) -> eval' cxt vvclos
          where
            env' = Map.insert x (VVClos a (cxtEnv cxt)) env
            vvclos = VVClos e env'
        (VInt _) -> throwError "Integer value cannot be applied, expected function value"
        _ -> throwError "Call by name "
  EAdd e e' -> do
    iv <- intValue =<< eval cxt e
    iv' <- intValue =<< eval cxt e'
    return $ VInt (iv + iv')
  ESub e e' -> do
    iv <- intValue =<< eval cxt e
    iv' <- intValue =<< eval cxt e'
    return $ VInt (iv - iv')
  ELt e e' -> do
    v <- intValue =<< eval cxt e
    v' <- intValue =<< eval cxt e'
    return $ if v < v' then VInt 1 else VInt 0
  EIf c t e -> do
    ic' <- intValue =<< eval cxt c
    if ic' == 1
      then eval cxt t
      else eval cxt e

-- | Evaluate VVClos values again
eval' :: Cxt -> Value -> Err Value
eval' cxt (VVClos e env) = eval cxt {cxtEnv = env} e
eval' cxt v = return v

apply :: Cxt -> Value -> Value -> Err Value
apply cxt f v =
  case f of
    VInt {} -> throwError "Integer value cannot be applied, expected function value"
    VClos x e env -> eval cxt {cxtEnv = Map.insert x v env} e