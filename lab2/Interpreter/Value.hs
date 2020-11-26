{-# LANGUAGE LambdaCase #-}

module Interpreter.Value where

import CMM.Abs (AddOp (..), CmpOp (..), Id (..), MulOp (..), Type (..))
import CMM.Print (printTree)
import Data.Map (Map)
import qualified Data.Map as Map

data Val
  = VBool Bool
  | VInt Int
  | VDouble Double
  | VVoid
  deriving (Eq)

instance Show Val where
  show (VBool b) = show b
  show (VInt b) = show b
  show (VDouble b) = show b
  show VVoid = "<void>"

--
cast :: Type -> Val -> Val
cast Type_double (VInt i) = VDouble $ fromIntegral i
cast Type_double v@(VDouble _) = v
cast Type_int v@(VInt _) = v
cast Type_bool v@(VBool _) = v
cast typ v = error $ "Typechecker bug: cannot cast " ++ show v ++ " to " ++ printTree typ

isTrue :: Val -> Bool
isTrue (VBool True) = True
isTrue _ = False

isFalse :: Val -> Bool
isFalse (VBool False) = True
isFalse _ = False

incr :: Val -> Val
incr (VDouble x) = VDouble $ x + 1
incr (VInt x) = VInt $ x + 1
incr _ = error "Typechecker bug: cannot increment non-numeric value"

decr :: Val -> Val
decr (VDouble x) = VDouble $ x - 1
decr (VInt x) = VInt $ x - 1
decr _ = error "Typechecker bug: cannot decrement non-numeric value"

add :: AddOp -> Val -> Val -> Val
add op (VDouble x) (VDouble y) = VDouble $ if op == OPlus then x + y else x - y
add op (VInt x) (VInt y) = VInt $ if op == OPlus then x + y else x - y
add _ _ _ = error "Typechecker bug: cannot add non-numeric values"

mul :: MulOp -> Val -> Val -> Val
mul op (VDouble x) (VDouble y) = VDouble $ if op == OTimes then x * y else x / y
mul op (VInt x) (VInt y) = VInt $ if op == OTimes then x * y else x `div` y
mul _ _ _ = error "Typechecker bug: cannot multiply non-numeric values"

comp :: CmpOp -> Val -> Val -> Val
comp OEq a b = VBool (a == b)
comp ONEq a b = VBool (a /= b)
comp op (VInt x) (VInt y) = VBool (comparator op x y)
comp op (VDouble x) (VDouble y) = VBool (comparator op x y)

comparator :: Ord a => CmpOp -> a -> a -> Bool
comparator = \case
  OLt -> (<)
  OLtEq -> (<=)
  OGt -> (>)
  OGtEq -> (>=)
  OEq -> (==)
  ONEq -> (/=)

builtIns :: Map Id ([Val] -> IO Val)
builtIns =
  Map.fromList
    [ (Id "printInt", printInt),
      (Id "printDouble", printDouble),
      (Id "readInt", readInt),
      (Id "readDouble", readDouble)
    ]
  where
    printInt [VInt i] = print i >> return VVoid
    printDouble [VDouble i] = print i >> return VVoid
    readInt _ = VInt <$> readLn
    readDouble _ = VDouble <$> readLn