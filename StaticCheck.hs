import Grammar.AbsGrammar
import Grammar.PrintGrammar
import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.State
import Data.List as L

checkErr :: Print a => a -> String -> String
checkErr tree = shows "Static check error:\n" . shows (printTree tree) . shows "\n--- "

type VarMap = M.Map Ident Type

data Env = Env {
  vars :: M.Map Ident Type,
  rtype :: Maybe Type,
  defined :: [Ident]
} deriving Show

type Checker a = StateT Env (Except String) a

stmtChecker :: Stmt -> Checker ()
stmtChecker s@(SDef n e) = do
  Env v r d <- get
  when (elem n d) $ throwError $ checkErr s "variable names must be unique in a block"
  t <- exprChecker e
  put $ Env (M.insert n t v) r (n:d)
stmtChecker SEmpty = return ()
stmtChecker (SBlock (Block (stmts))) = do
  env <- get
  mapM_ stmtChecker stmts
  put env
stmtChecker s@(SAss n e) = do
  t' <- exprChecker e
  vs <- gets vars
  case M.lookup n vs of
    Nothing -> throwError $ checkErr s "variable not defined"
    Just t -> when (t' /= t) $ throwError $ checkErr s "unexpected expression type"
stmtChecker s@(SRet e) = do
  t' <- exprChecker e
  mt <- gets rtype
  case mt of
    Nothing -> throwError $ checkErr SVRet "return statement outside function body"
    Just t -> when (t' /= t) $ throwError $ checkErr SVRet "returning unexpected type"
stmtChecker SVRet = do
  mt <- gets rtype
  case mt of
    Nothing -> throwError $ checkErr SVRet "return statement outside function body"
    Just t -> when (t /= TVoid) $ throwError $ checkErr SVRet "returning unexpected type"
stmtChecker s@(SIf e b) = do
  condBoolChecker e 
  stmtChecker $ SBlock b
stmtChecker s@(SIfElse e b c) = do
  condBoolChecker e 
  ifContChecker c
stmtChecker s@(SWhile e b) = stmtChecker $ SIf e b
stmtChecker s@(SExpr e) = void $ exprChecker e

ifContChecker :: IfCont -> Checker ()
ifContChecker s@(SElif e b) = stmtChecker $ SIf e b
ifContChecker s@(SElifElse e b c) = stmtChecker $ SIfElse e b c
ifContChecker (SElse b) = stmtChecker $ SBlock b

condBoolChecker :: Expr -> Checker ()
condBoolChecker e = do
  t <- exprChecker e
  when (t /= TBool) $ throwError $ checkErr e "conditional expression is not of type bool"

exprChecker :: Expr -> Checker Type
exprChecker e@(EVar n) = do
  v <- gets vars
  case M.lookup n v of
    Nothing -> throwError $ checkErr e "variable not defined"
    Just t -> return t
exprChecker (EInt _) = return TInt
exprChecker ETrue = return TBool
exprChecker EFalse = return TBool
exprChecker (EStr _) = return TStr
exprChecker e@(EFunc args t b@(Block stmts)) = do
  unless argsUnique $ throwError $ checkErr e "argument names must be unique"
  unless (t == TVoid) retChecker
  Env v r d <- get
  put $ Env (foldr insertArg v args) (Just t) (foldr insertArgName d args)
  stmtChecker $ SBlock b
  put $ Env v r d
  return $ TFunc params t
  where
    names :: [String]
    names = map (\(Arg (Ident n) _) -> n) args
    argsUnique :: Bool
    argsUnique = length (L.nub names) == length names
    insertArg :: Arg -> VarMap -> VarMap
    insertArg (Arg n (VarPar t)) = M.insert n t
    insertArg (Arg n (ValPar t)) = M.insert n t
    insertArgName :: Arg -> [Ident] -> [Ident]
    insertArgName (Arg n _) = (n:)
    retChecker :: Checker ()
    retChecker = do
      when (null stmts) $ throwError $ checkErr e "non-void function has to end with a return statement"
      case last stmts of
        SRet _ -> return ()
        SVRet -> return ()
        _ -> throwError $ checkErr e "non-void function has to end with a return statement"
    params :: [ParType]
    params = map (\(Arg _ p) -> p) args
exprChecker e@(EApp efun eargs) = do
  ft <- exprChecker efun
  case ft of
    TFunc ps rt -> do
        ats <- mapM exprChecker eargs
        mapM_ typeMatcher $ zip ps ats
        return rt
    _ -> throwError $ checkErr e "expression is not a function"
  where
    typeMatcher :: (ParType, Type) -> Checker ()
    typeMatcher ((VarPar t), t') = when (t' /= t) $ throwError $ checkErr e "unexpected argument type"
    typeMatcher ((ValPar t), t') = when (t' /= t) $ throwError $ checkErr e "unexpected argument type"
exprChecker (ENeg e) = exactTypeChecker TInt [e]
exprChecker (ENot e) = exactTypeChecker TBool [e]
exprChecker (EMul e1 _ e2) = exactTypeChecker TInt [e1, e2]
exprChecker (EAdd e1 _ e2) = exactTypeChecker TInt [e1, e2]
exprChecker (ERel e1 _ e2) = exactTypeChecker TInt [e1, e2]
exprChecker (EAnd e1 e2) = exactTypeChecker TBool [e1, e2]
exprChecker (EOr e1 e2) = exactTypeChecker TBool [e1, e2]

exactTypeChecker :: Type -> [Expr] -> Checker Type
exactTypeChecker t e = do
  mapM_ checker e
  return t
  where
    checker :: Expr -> Checker ()
    checker e = do
      t' <- exprChecker e
      when (t' /= t) $ throwError $ checkErr e "unexpected expression type"
