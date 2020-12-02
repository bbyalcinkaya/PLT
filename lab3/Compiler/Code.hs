{-# LANGUAGE LambdaCase #-}

module Compiler.Code where

import CMM.Abs

data FunType = FunType Type [Type] deriving (Show, Eq)

newtype Label = L Int deriving (Show, Eq)

suc :: Label -> Label
suc (L x) = L $ x + 1

type Address = Int

--newtype Address = Address Int deriving (Show, Eq)

data ArithOp = ArithAdd AddOp | ArithMul MulOp deriving (Show, Eq, Read, Ord)

data Fun = Fun Id FunType deriving (Show, Eq)

data Code
  = Store Type Address
  | Load Type Address
  | IConst Int
  | DConst Double
  | Dup Type
  | Pop Type
  | Return Type
  | Call Fun
  | Label Label
  | Goto Label
  | If CmpOp Label -- if top of the stack is op jumpt to label ?
  | IfCmp Type CmpOp Label -- if prev op top ...
  | DCmp
  | Inc Type Address Int
  | Arith Type ArithOp
  | I2D -- int to double conv.
  | Comment String
  deriving (Show)

class Size a where
  size :: a -> Int

instance Size Type where
  size t = case t of
    Type_int -> 1
    Type_double -> 2
    Type_bool -> 1
    Type_void -> 0

instance (Size a, Size b) => Size (a, b) where
  size (a, b) = size a + size b

instance Size a => Size [a] where
  size = sum . map size

instance Size FunType where
  size (FunType t ts) = size ts - size t

instance Size Fun where
  size (Fun _ ft) = size ft

class ToJVM a where
  toJVM :: a -> String

instance ToJVM Type where
  toJVM t = case t of
    Type_int -> "I"
    Type_void -> "V"
    Type_double -> "D"
    Type_bool -> "Z"

instance ToJVM FunType where
  toJVM (FunType t ts) = "(" ++ (toJVM =<< ts) ++ ")" ++ toJVM t

instance ToJVM Fun where
  toJVM (Fun (Id f) t) = f ++ toJVM t

instance ToJVM Label where
  toJVM (L label) = "L" ++ show label

instance ToJVM Code where
  toJVM = \case
    Store t n -> prefix t ++ "store" ++ sep 3 n ++ show n
    Load t n -> prefix t ++ "load" ++ sep 3 n ++ show n
    Return t -> prefix t ++ "return"
    Call f -> "invokestatic " ++ toJVM f
    DConst d -> "ldc2_w " ++ show d
    IConst i
      | i == -1 -> "iconst_m1"
      | i >= 0 && i <= 5 -> "iconst_" ++ show i
      | i >= -128 && i <= 127 -> "bipush " ++ show i -- -128 <= x <= 127
      | otherwise -> "ldc " ++ show i
    Dup Type_double -> "dup2"
    Dup _ -> "dup"
    Pop Type_double -> "pop2"
    Pop _ -> "pop"
    Label l -> toJVM l ++ ":"
    Goto l -> "goto " ++ toJVM l
    If op l -> "if" ++ toJVM op ++ " " ++ toJVM l
    c@(IfCmp Type_double _ _) -> impossible c
    c@(IfCmp Type_void _ _) -> impossible c
    IfCmp _ op l -> "if_icmp" ++ toJVM op ++ " " ++ toJVM l
    DCmp -> "dcmpg"
    Inc Type_int a k -> "iinc " ++ show a ++ " " ++ show k
    c@Inc {} -> impossible c
    Arith t op -> prefix t ++ toJVM op
    I2D -> "i2d"
    Comment "" -> ""
    Comment s -> ";; " ++ s

instance ToJVM ArithOp where
  toJVM (ArithAdd OPlus) = "add"
  toJVM (ArithMul OTimes) = "mul"
  toJVM (ArithMul ODiv) = "div"
  toJVM op = impossible op

instance ToJVM CmpOp where
  toJVM OLt = "lt"
  toJVM OLtEq = "le"
  toJVM OGt = "gt"
  toJVM OGtEq = "ge"
  toJVM OEq = "eq"
  toJVM ONEq = "ne"

sep :: Int -> Int -> String
sep i x
  | x >= 0 && x <= i = "_"
  | otherwise = " "

impossible :: Show t => t -> a
impossible c = error $ "impossible happened " ++ show c

prefix :: Type -> String
prefix Type_int = "i"
prefix Type_double = "d"
prefix _ = ""