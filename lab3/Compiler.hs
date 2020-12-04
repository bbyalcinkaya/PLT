-- Optional: turn on warnings.
-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | Compiler for C--, producing symbolic JVM assembler.
module Compiler where

import Annotated
import CMM.Abs hiding (Program)
import CMM.Print (printTree)
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
    builtin =
      [ (Id "printInt", Fun (Id "Runtime/printInt") $ FunType Type_void [Type_int]),
        (Id "printDouble", Fun (Id "Runtime/printDouble") $ FunType Type_void [Type_double]),
        (Id "readInt", Fun (Id "Runtime/readInt") $ FunType Type_int []),
        (Id "readDouble", Fun (Id "Runtime/readDouble") $ FunType Type_double [])
      ]

compileDef :: Sig -> Def -> [String]
compileDef sig0 def@(DFun t id args stmts) =
  concat
    [ [ "",
        ".method public static " ++ toJVM (Fun id $ funType def)
      ],
      [ ".limit locals " ++ show (limitLocals st),
        ".limit stack " ++ show (limitStack st)
      ],
      map (indent . toJVM) $ reverse (Env.code st),
      [ "",
        ".end method"
      ]
    ]
  where
    st = execState (compileFun t args stmts) $ Env.initEnv sig0

indent :: String -> String
indent str = if null str then str else "\t" ++ str

compileFun :: Type -> [Arg] -> [Stm] -> State Env ()
compileFun t args stmts = do
  mapM_ (\(ADecl t' x) -> newVar x t') args
  mapM_ compileStm stmts

  -- prevents "Falling off the end of the code"
  case t of
    Type_bool -> compileStm $ SReturn $ EBool LFalse
    Type_int -> compileStm $ SReturn $ EInt 0
    Type_double -> compileStm $ SReturn $ EDouble 0.0
    Type_void -> emit $ Return Type_void

compileStm :: Stm -> State Env ()
compileStm s0 = do
  -- print statement as a comment
  let cText = printTree s0
  mapM_ comment (lines cText)

  case s0 of
    SExp exp -> do
      t <- compileExp exp
      emit $ Pop t
    SDecls typ ids -> do
      mapM_ (\id -> newVar id typ) ids
    SInit typ id exp -> do
      compileStm $ SDecls typ [id]
      compileStm $ SExp $ EAss id exp
    SReturn e -> do
      t <- compileExp e
      emit $ Return t
    SWhile exp stm -> do
      beginLabel <- newLabel
      endLabel <- newLabel

      emit $ Label beginLabel
      compileExp exp
      emit $ If OEq endLabel

      withBlock $ compileStm stm

      emit $ Goto beginLabel
      emit $ Label endLabel
    SBlock stms -> withBlock $ mapM_ compileStm stms
    SIfElse exp stm1 stm2 -> do
      compileExp exp
      falseLabel <- newLabel
      endLabel <- newLabel
      emit $ If OEq falseLabel

      withBlock $ compileStm stm1
      emit $ Goto endLabel

      emit $ Label falseLabel
      withBlock $ compileStm stm2

      emit $ Label endLabel

comment :: String -> State Env ()
comment = emit . Comment

withBlock :: State Env a -> State Env a
withBlock c = do
  modify Env.push
  r <- c
  modify Env.pop
  return r

compileExp :: Exp -> State Env Type
compileExp = \case
  EInt i -> do
    emit $ IConst $ fromIntegral i
    return Type_int
  EDouble d -> do
    emit $ DConst $ d
    return Type_double
  EBool b -> do
    emit $ IConst b'
    return Type_bool
    where
      b' = if b == LTrue then 1 else 0
  ECast Type_double exp -> do
    t <- compileExp exp
    if t == Type_int
      then emit I2D
      else return ()
    return Type_double
  ECast _ e -> compileExp e -- ECast Type_bool (EBool True)
  EId id -> do
    (n, t) <- lookupVar id
    emit $ Load t n
    return t
  EApp id exps -> do
    fun@(Fun _ (FunType t _)) <- lookupFun id
    mapM_ compileExp exps
    emit $ Call fun
    return t
  EPost id op -> do
    (n, t) <- lookupVar id
    emit $ Load t n
    emit $ Inc t n (if op == OInc then 1 else -1)
    return t
  EPre op id -> do
    (n, t) <- lookupVar id
    emit $ Inc t n (if op == OInc then 1 else -1)
    emit $ Load t n
    return t
  EMul expl op expr -> do
    t1 <- compileExp expl
    _ <- compileExp expr
    emit $ Arith t1 (ArithMul op)
    return t1
  EAdd expl op expr -> do
    t1 <- compileExp expl
    _ <- compileExp expr
    emit $ Arith t1 (ArithAdd op)
    return t1
  ECmp expl op expr -> do
    t <- compileExp expl
    compileExp expr

    trueLabel <- newLabel
    falseLabel <- newLabel
    emit $ IfCmp t op trueLabel
    emit $ IConst 0
    emit $ Goto falseLabel

    emit $ Label trueLabel
    emit $ IConst 1

    emit $ Label falseLabel
    return Type_bool

  -- lazy
  EAnd expl expr -> do
    compileExp expl

    falseLabel <- newLabel
    trueLabel <- newLabel
    emit $ If OEq falseLabel

    compileExp expr
    emit $ Goto trueLabel

    emit $ Label falseLabel
    emit $ IConst 0
    emit $ Label trueLabel
    return Type_bool
  EOr expl expr -> do
    compileExp expl

    trueLabel <- newLabel
    falseLabel <- newLabel
    emit $ If ONEq trueLabel

    compileExp expr
    emit $ Goto falseLabel

    emit $ Label trueLabel
    emit $ IConst 1
    emit $ Label falseLabel
    return Type_bool
  EAss id exp -> do
    (n, t) <- lookupVar id
    compileExp exp
    emit $ Store t n
    emit $ Load t n
    return t

-- e -> error $ "Not yet implemented: compileExp " ++ show e

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
  Store t _ -> decStack t
  Load t _ -> incStack t
  IConst _ -> incStack Type_int
  DConst _ -> incStack Type_double
  Dup t -> incStack t
  Pop t -> decStack t
  Return t -> decStack t
  Call f -> decStack f
  Label {} -> return ()
  Goto {} -> return ()
  If _ _ -> decStack Type_int
  IfCmp t _ _ -> decStack t >> decStack t
  DCmp -> decStack Type_double >> decStack Type_double >> incStack Type_int
  Inc {} -> return ()
  Arith t _ -> decStack t
  I2D -> decStack Type_int >> incStack Type_double
  Comment _ -> return ()

incStack :: Size t => t -> State Env ()
incStack t = modStack (size t)

decStack :: Size t => t -> State Env ()
decStack t = modStack (0 - size t)

modStack :: Int -> State Env ()
modStack n = do
  new <- (n +) <$> gets currentStack
  modify $ \env -> env {currentStack = new}
  old <- gets limitStack
  when (new > old) $
    modify $ \env -> env {limitStack = new}

newVar :: Id -> Type -> State Env ()
newVar id typ = modify $ Env.newVar id typ

lookupVar :: Id -> State Env (Address, Type)
lookupVar id = Env.lookupVar id <$> get

lookupFun :: Id -> State Env Fun
lookupFun id = Env.lookupFun id <$> get

newLabel :: State Env Label
newLabel = do
  l <- gets nextLabel
  modify $ \env -> env {nextLabel = suc l}
  return l
