module Executor where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except
import Grammar.AbsGrammar
import Errors
import State

data StmtRet = RNone | RVal Value

stmtExecutor :: Stmt -> Executor StmtRet
stmtExecutor (SDef n e) = do
  val <- exprExecutor e
  modify $ defVal n val
  return RNone

stmtExecutor SEmpty = return RNone

stmtExecutor (SBlock (Block ss)) = scopeVars f
  where
    f :: Executor StmtRet
    f = do
      ret <- foldM innerExecutor RNone ss
      return ret
    innerExecutor :: StmtRet -> Stmt -> Executor StmtRet
    innerExecutor RNone s = stmtExecutor s
    innerExecutor ret _ = return ret

stmtExecutor (SAss n e) = do
  val <- exprExecutor e
  modify $ assVal n val
  return RNone

stmtExecutor (SRet e) = do
  val <- exprExecutor e
  return $ RVal val

stmtExecutor SVRet = return $ RVal VVoid

stmtExecutor (SIf e b) = do
  VBool val <- exprExecutor e
  if val
    then stmtExecutor $ SBlock b
    else return RNone

stmtExecutor (SIfElse e b c) = do
  VBool val <- exprExecutor e
  if val
    then stmtExecutor $ SBlock b
    else ifContExecutor c

stmtExecutor s@(SWhile e b) = do
  VBool val <- exprExecutor e
  if val
    then do
      ret <- stmtExecutor $ SBlock b
      case ret of
        RNone -> stmtExecutor s
        v -> return $ v
    else return RNone

stmtExecutor (SExpr e) = do
  exprExecutor e
  return RNone

ifContExecutor :: IfCont -> Executor StmtRet
ifContExecutor (SElif e b) = stmtExecutor $ SIf e b
ifContExecutor (SElifElse e b c) = stmtExecutor $ SIfElse e b c
ifContExecutor (SElse b) = stmtExecutor $ SBlock b

exprExecutor :: Expr -> Executor Value
exprExecutor (EVar n) = do
  val <- gets $ getVal n
  return val

exprExecutor (EInt i) = return $ VInt i

exprExecutor ETrue = return $ VBool True

exprExecutor EFalse = return $ VBool False

exprExecutor (EStr str) = return $ VStr str

exprExecutor (EFunc as _ b) = do
  v <- gets $ vars
  return $ VFunc $ scopeVars . ex v
    where
      ex :: VarMap -> [Expr] -> Executor Value
      ex v es = do
        v' <- foldM putArg v $ zip as es
        modify $ putVars v'
        modify $ defVal (Ident "this") $ VFunc $ scopeVars . ex v
        res <- stmtExecutor $ SBlock b
        case res of
          RVal v -> return v
          RNone -> return VVoid
      putArg :: VarMap -> (Arg, Expr) -> Executor VarMap
      putArg v (Arg n (VarPar _), EVar n') = do
        l <- gets $ getLoc n'
        return $ M.insert n l v
      putArg v (Arg n _, e) = do
        val <- exprExecutor e
        modify $ defVal n val
        l <- gets $ getLoc n
        return $ M.insert n l v

exprExecutor (EApp f vals) = do
  VFunc ex <- exprExecutor f
  ex vals

exprExecutor (ENeg e) = do
  VInt val <- exprExecutor e
  return $ VInt $ negate val

exprExecutor (ENot e) = do
  VBool val <- exprExecutor e
  return $ VBool $ not val

exprExecutor (EMul e1 OTimes e2) = do
  VInt val1 <- exprExecutor e1
  VInt val2 <- exprExecutor e2
  return $ mulOp OTimes val1 val2

exprExecutor (EMul e1 ODiv e2) = do
  VInt val1 <- exprExecutor e1
  VInt val2 <- exprExecutor e2
  when (val2 == 0) $ throwError $ runtimeErr "division by 0"
  return $ mulOp ODiv val1 val2

exprExecutor (EMul e1 OMod e2) = do
  VInt val1 <- exprExecutor e1
  VInt val2 <- exprExecutor e2
  when (val2 == 0) $ throwError $ runtimeErr "modulo by 0"
  return $ mulOp OMod val1 val2

exprExecutor (EAdd e1 op e2) = do
  VInt val1 <- exprExecutor e1
  VInt val2 <- exprExecutor e2
  return $ addOp op val1 val2

exprExecutor (ERel e1 op e2) = do
  VInt val1 <- exprExecutor e1
  VInt val2 <- exprExecutor e2
  return $ relOp op val1 val2

exprExecutor (EAnd e1 e2) = do
  VBool val1 <- exprExecutor e1
  if val1
    then exprExecutor e2
    else return $ VBool False

exprExecutor (EOr e1 e2) = do
  VBool val1 <- exprExecutor e1
  if val1
    then return $ VBool True
    else exprExecutor e2

mulOp :: MulOp -> Integer -> Integer -> Value
mulOp op x y = VInt $ getOp op x y
  where
    getOp :: MulOp -> Integer -> Integer -> Integer
    getOp OTimes = (*)
    getOp ODiv = div
    getOp OMod = mod

addOp :: AddOp -> Integer -> Integer -> Value
addOp op x y = VInt $ getOp op x y
  where
    getOp :: AddOp -> Integer -> Integer -> Integer
    getOp OPlus = (+)
    getOp OMinus = (-)

relOp :: RelOp -> Integer -> Integer -> Value
relOp op x y = VBool $ getOp op x y
  where
    getOp :: RelOp -> Integer -> Integer -> Bool
    getOp OLt = (<)
    getOp OLte = (<=)
    getOp OGt = (>)
    getOp OGte = (>=)
    getOp OEq = (==)
    getOp ONeq = (/=)
