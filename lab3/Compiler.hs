-- Optional: turn on warnings.
-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | Compiler for C--, producing symbolic JVM assembler.
module Compiler where

import Annotated
import CMM.Abs hiding (Program)
import Compiler.Code
import Compiler.Environment (Env (..), Sig)
import qualified Compiler.Environment as Env
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe

-- | Entry point.
compile ::
  -- | Class name.
  String ->
  -- | Type-annotated program.
  Program ->
  -- | Generated jasmin source file content.
  String
compile name _prg@(PDefs defs) = unlines $ concat $ header : map (compileDef sig0) defs
  where
    sig0 = Map.fromList $ builtin ++ map sigEntry defs
    sigEntry def@(DFun _ f@(Id x) _ _) = (,) f $ Fun (Id $ name ++ "/" ++ x) $ funType def
    funType (DFun typ _ args _) = FunType typ $ map (\(ADecl t _) -> t) args
    header :: [String]
    header =
      [ ";; BEGIN HEADER",
        "",
        ".class public " ++ name,
        ".super java/lang/Object",
        "",
        ".method public <init>()V",
        "  .limit locals 1",
        "",
        "  aload_0",
        "  invokespecial java/lang/Object/<init>()V",
        "  return",
        "",
        ".end method",
        "",
        ".method public static main([Ljava/lang/String;)V",
        "  .limit locals 1",
        "  .limit stack  1",
        "",
        "  invokestatic " ++ name ++ "/main()I",
        "  pop",
        "  return",
        "",
        ".end method",
        "",
        ";; END HEADER"
      ]
    builtin :: [(Id, Fun)]
    builtin = undefined

compileDef :: Sig -> Def -> [String]
compileDef = undefined

compileFun :: Type -> [Arg] -> [Stm] -> State Env ()
compileFun = undefined

compileStm :: Stm -> State Env ()
compileStm = undefined

compileExp :: Exp -> State Env ()
compileExp = undefined

emit :: Code -> State Env ()
emit (Store Type_void _) = return ()
emit (Load Type_void _) = return ()
emit (Dup Type_void) = return ()
emit (Pop Type_void) = return ()
emit (Inc t@Type_double a k) = do
  emit $ Load t a
  emit $ DConst $ fromIntegral k
  emit $ Arith t (ArithAdd OPlus)
  emit $ Store t a
emit (IfCmp Type_double o l) = do
  emit $ DCmp
  emit $ If o l
emit c = do
  modify $ \env@Env {code = cs} -> env {code = c : cs}
  adjustStack c

adjustStack :: Code -> State Env ()
adjustStack = \case
  Store t _   -> decStack t
  Load t _    -> incStack t
  IConst _    -> incStack Type_int
  DConst _    -> incStack Type_double
  Dup t       -> incStack t
  Pop t       -> decStack t
  Return t    -> decStack t
  Call f      -> decStack f
  Label{}     -> return ()
  Goto{}      -> return ()
  If _ _      -> decStack Type_int
  IfCmp t _ _ -> decStack t >> decStack t
  DCmp        -> decStack Type_double >> decStack Type_double >> incStack Type_int
  Inc{}       -> return ()
  Arith t _   -> decStack t
  I2D         -> decStack  Type_int >> incStack Type_double
  Comment _   -> return ()

incStack :: Size t => t -> State Env ()
incStack t = modStack (size t)

decStack :: Size t => t -> State Env ()
decStack t = modStack (0 - size t)

modStack :: Int -> State Env ()
modStack n = do
  new <- (n +) <$> gets currentStack
  modify $ \ env -> env { currentStack = new }
  old <- gets limitStack
  when (new > old) $
    modify $ \ env -> env { limitStack = new }
    
lookupVar :: Id -> State Env (Address, Type)
lookupVar id = Env.lookupVar id <$> get

lookupFun :: Id -> State Env Fun
lookupFun id = Env.lookupFun id <$> get

newLabel :: State Env Label
newLabel = do
  l <- gets nextLabel
  modify $ \env -> env {nextLabel = suc l}
  return l