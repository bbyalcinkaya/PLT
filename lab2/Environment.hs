module Environment where

import CMM.Abs
import CMM.ErrM
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception.Base (throw)

type Environment ft vt = (Signatures ft, [Context vt])

type Signatures ft = Map Id ft

type Context vt = Map Id vt

lookupVar :: Id -> Environment ft vt -> Err vt
lookupVar id (_, []) = errorUndefinedVar id
lookupVar id (sig, context : rest) = case Map.lookup id context of
  Just x -> Ok x
  Nothing -> lookupVar id (sig, rest)

-- define a new variable in the current scope
newVar :: Id -> v -> Environment f v -> Environment f v
newVar id val (sig, context : rest) = (sig, Map.insert id val context : rest)
newVar _ _ (_, []) = throw $ userError "cannot initialize a variable. context stack is empty" -- interpreter/typechecker bug

-- update an existing variable in context stack
updateVar :: Id -> v -> Environment f v -> Err (Environment f v)
updateVar id val (sig, ctxs) = (,) sig <$> loop ctxs
  where
    loop [] = errorUndefinedVar id
    loop (ctx : rest) = case Map.lookup id ctx of
      Nothing -> (ctx :) <$> loop rest
      Just _ -> Ok $ Map.insert id val ctx : rest

lookupFun :: Id -> Environment ft vt -> Err ft
lookupFun id (sig, _) = case Map.lookup id sig of
  Just x -> Ok x
  Nothing -> errorUndefinedFunction id

newFun :: Id -> ft -> Environment ft vt -> Environment ft vt
newFun id val (sig, context) = (Map.insert id val sig, context)

newBlock :: Environment ft vt -> Environment ft vt
newBlock (env, context) = (env, Map.empty : context)

dropBlock :: Environment f v -> Environment f v
dropBlock (_, []) = throw $ userError "cannot drop block from empty stack" -- interpreter/typechecker bug
dropBlock (env, _ : rest) = (env, rest)

emptyEnvironment :: Environment f v
emptyEnvironment = (Map.empty, [])

emptyContext :: Environment f v -> Environment f v'
emptyContext (sig, _) = (sig, [])

errorUndefinedVar :: Id -> Err a
errorUndefinedVar (Id var) = Bad $ "undefined variable " ++ var
errorUninitializedVar :: Id -> Err a
errorUninitializedVar (Id var) = Bad $ "uninitialized variable " ++ var
errorUndefinedFunction :: Id -> Err a
errorUndefinedFunction (Id var) = Bad $ "undefined function " ++ var