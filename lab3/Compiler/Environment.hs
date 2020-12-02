module Compiler.Environment where

import Annotated
import CMM.Abs (Id, Type)
import CMM.Print (printTree)
import Compiler.Code
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

type Sig = Map Id Fun

type Block = Map Id (Address, Type)

type Ctx = [Block]

type Output = [Code]

data Env = Env
  { sig :: Sig,
    vars :: Ctx,
    currentLocals :: Int, -- current size of locals
    limitLocals :: Int, -- max size for locals (???)
    currentStack :: Int, -- current stack size
    limitStack :: Int, -- max stack size encountered
    nextLabel :: Label,
    code :: Output --
  }

initEnv :: Sig -> Env
initEnv s =
  Env
    { sig = s,
      vars = [Map.empty],
      currentLocals = 0,
      limitLocals = 0,
      currentStack = 0,
      limitStack = 0,
      nextLabel = L 0,
      code = []
    }

lookupVar :: Id -> Env -> (Address, Type)
lookupVar id env = loop $ vars env
  where
    loop [] = error $ "unbound variable " ++ printTree id
    loop (b : bs) = case Map.lookup id b of
      Nothing -> loop bs
      Just x -> x

lookupFun :: Id -> Env -> Fun
lookupFun id = fromJust . Map.lookup id . sig

newVar :: Id -> Type -> Env -> Env
newVar id typ env =
  env
    { vars = ctx' : rest,
      currentLocals = curLocals,
      limitLocals = max (limitLocals env) curLocals
    }
  where
    addr = currentLocals env
    curLocals = addr + size typ
    (ctx : rest) = vars env
    ctx' = Map.insert id (addr, typ) ctx

newFun :: Id -> FunType -> Env -> Env
newFun = undefined

push :: Env -> Env
push env@Env {vars = vs} = env {vars = Map.empty : vs}

pop :: Env -> Env
pop env@Env {vars = v : vs} =
  env
    { vars = vs,
      currentLocals = currentLocals env - scopeSize
    }
  where
    scopeSize = Map.foldl (\acc (_, t) -> acc + size t) 0 v

