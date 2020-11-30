module Compiler.Environment where

import Data.Map (Map)
import qualified Data.Map as Map

import Annotated
import CMM.Abs (Id)
import Compiler.Instruction

type FunType = ([Type], Type)
type Address = Int


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

extend :: 

