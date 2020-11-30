module Compiler.Instruction where

-- | Jasmin types: I: int D: double V: void Z: boolean
data Type
  = I -- int
  | D -- double
  | V -- void
  | Z -- boolean
  deriving (Show, Eq)

type Label = String

data Instruction
  -- constants
  = Iconst_m1
  | Iconst_0
  | Iconst_1
  | Iconst_2
  | Iconst_3
  | Iconst_4
  | Iconst_5
  | Dconst_0
  | Dconst_1
  | Bipush Int -- -128 <= x <= 127 
  | Ldc Int
  | LdcString String
  | Ldc2_w Double
  -- 
  | Goto Label
  | If_icmpeq Label
  | If_icmpne  Label
  | If_icmple  Label
  | If_icmpge  Label
  | If_icmplt  Label
  | If_icmpgt  Label
  | Ifeq  Label
  | Ifne  Label
  | Ifle  Label
  | Ifge  Label
  | Iflt  Label
  | Ifgt  Label
  | Ifnonnull Label
  | Ifnull Label
  
  -- Returns
  | Ireturn
  | Dreturn
  | Return -- Void

  | Iadd
  | Imul
  | Idiv
  | Pop
  | Dadd
  | Dmul
  | Ddiv
  | D2i

  | Iload_0
  | Iload_1
  | Iload_2
  | Iload_3
  | Iload Int
  | Istore Int
  | Istore_0
  | Istore_1
  | Istore_2
  | Istore_3

  | Dload_0
  | Dload_1
  | Dload_2
  | Dload_3
  | Dload Double
  | Dstore Double
  | Dstore_0
  | Dstore_1
  | Dstore_2
  | Dstore_3

  | Invokestatic String -- TODO this should not be String
  -- http://jasmin.sourceforge.net/instructions.html