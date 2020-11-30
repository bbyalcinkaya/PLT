module Compiler.Environment where

import Data.Map (Map)
import qualified Data.Map as Map

import Annotated
import CMM.Abs (Id)
import Compiler.Instruction ( Type, Instruction )

type FunType = ([Type], Type)
type Address = Int
type Size = Int

data Env = Env {
  sig :: Map Id FunType,
  vars :: [Map Id Int],
  maxVar :: Int,
  code :: [Instruction]
}

lookupVar :: Id -> Env -> Address
lookupVar = undefined

lookupFun :: Id -> Env -> FunType
lookupFun = undefined

newVar :: Id -> Size -> Env -> Env 
newVar = undefined

newFun :: Id -> FunType -> Env -> Env
newFun = undefined

push :: Env -> Env
push env = undefined

pop :: Env -> Env
pop env = undefined

empty :: Env -> Env
empty env = undefined

label :: String
label = undefined