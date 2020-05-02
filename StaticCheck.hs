import Grammar.AbsGrammar
import Grammar.PrintGrammar
import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.State
import Data.List as L
import Errors

type VarMap = M.Map Ident Type

data Env = Env {
  vars :: M.Map Ident Type,
  rtype :: Maybe Type,
  defined :: [Ident]
} deriving Show

type Checker a = StateT Env (Except Err) a

staticErr :: String -> Err
staticErr msg = Err [] $ "static check error: " ++ msg

withTraceback :: Print a => (a -> Checker b) -> (a -> Checker b)
withTraceback c s = catchError (c s) handler
  where
    handler :: Err -> Checker b
    handler e = throwError $ addContext s e

stmtChecker :: Stmt -> Checker ()
stmtChecker = withTraceback stmtChecker'
  where
    stmtChecker' :: Stmt -> Checker ()
    stmtChecker' (SDef n e) = do
      Env v r d <- get
      when (elem n d) $ throwError $ staticErr "variable names must be unique in a block"
      t <- exprChecker e
      put $ Env (M.insert n t v) r (n:d)
    stmtChecker' SEmpty = return ()
    stmtChecker' (SBlock (Block (stmts))) = do
      env <- get
      mapM_ stmtChecker stmts
      put env
    stmtChecker' (SAss n e) = do
      t' <- exprChecker e
      vs <- gets vars
      case M.lookup n vs of
        Nothing -> throwError $ staticErr "variable not defined"
        Just t -> when (t' /= t) $ throwError $ staticErr "unexpected expression type"
    stmtChecker' (SRet e) = do
      t' <- exprChecker e
      mt <- gets rtype
      case mt of
        Nothing -> throwError $ staticErr "return statement outside function body"
        Just t -> when (t' /= t) $ throwError $ staticErr "returning unexpected type"
    stmtChecker' SVRet = do
      mt <- gets rtype
      case mt of
        Nothing -> throwError $ staticErr "return statement outside function body"
        Just t -> when (t /= TVoid) $ throwError $ staticErr "returning unexpected type"
    stmtChecker' (SIf e b) = do
      condBoolChecker e
      stmtChecker $ SBlock b
    stmtChecker' (SIfElse e b c) = do
      stmtChecker' $ SIf e b
      ifContChecker c
    stmtChecker' (SWhile e b) = stmtChecker' $ SIf e b
    stmtChecker' (SExpr e) = void $ exprChecker e
    ifContChecker :: IfCont -> Checker ()
    ifContChecker (SElif e b) = stmtChecker' $ SIf e b
    ifContChecker (SElifElse e b c) = stmtChecker' $ SIfElse e b c
    ifContChecker (SElse b) = stmtChecker $ SBlock b
    condBoolChecker :: Expr -> Checker ()
    condBoolChecker e = do
      t <- exprChecker e
      when (t /= TBool) $ throwError $ staticErr "conditional expression is not of type bool"

exprChecker :: Expr -> Checker Type
exprChecker = withTraceback exprChecker'
  where
    exprChecker' (EVar n) = do
      v <- gets vars
      case M.lookup n v of
        Nothing -> throwError $ staticErr "variable not defined"
        Just t -> return t
    exprChecker' (EInt _) = return TInt
    exprChecker' ETrue = return TBool
    exprChecker' EFalse = return TBool
    exprChecker' (EStr _) = return TStr
    exprChecker' (EFunc args t b@(Block stmts)) = do
      unless argsUnique $ throwError $ staticErr "argument names must be unique"
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
          when (null stmts) $ throwError $ staticErr "non-void function has to end with a return statement"
          case last stmts of
            SRet _ -> return ()
            SVRet -> return ()
            _ -> throwError $ staticErr "non-void function has to end with a return statement"
        params :: [ParType]
        params = map (\(Arg _ p) -> p) args
    exprChecker' (EApp efun eargs) = do
      ft <- exprChecker efun
      case ft of
        TFunc ps rt -> do
            mapM_ typeMatcher $ zip ps eargs
            return rt
        _ -> throwError $ staticErr "expression is not a function"
      where
        typeMatcher :: (ParType, Expr) -> Checker ()
        typeMatcher ((VarPar t), e) = typeMatcher ((ValPar t), e)
        typeMatcher ((ValPar t), e) = do
          t' <- exprChecker e
          when (t' /= t) $ throwError $ staticErr "unexpected argument type"
    exprChecker' (ENeg e) = exactTypeChecker TInt [e]
    exprChecker' (ENot e) = exactTypeChecker TBool [e]
    exprChecker' (EMul e1 _ e2) = exactTypeChecker TInt [e1, e2]
    exprChecker' (EAdd e1 _ e2) = exactTypeChecker TInt [e1, e2]
    exprChecker' (ERel e1 _ e2) = exactTypeChecker TInt [e1, e2]
    exprChecker' (EAnd e1 e2) = exactTypeChecker TBool [e1, e2]
    exprChecker' (EOr e1 e2) = exactTypeChecker TBool [e1, e2]
    exactTypeChecker :: Type -> [Expr] -> Checker Type
    exactTypeChecker t e = do
      mapM_ checker e
      return t
      where
        checker :: Expr -> Checker ()
        checker e = do
          t' <- exprChecker e
          when (t' /= t) $ throwError $ staticErr "unexpected expression type"
