module Predefined where

import Control.Monad.Except
import Control.Monad.State
import Text.Read (readMaybe)
import Grammar.AbsGrammar
import Errors
import State

predefinedTypes :: [(Ident, Type)]
predefinedTypes = [
  (Ident "printInt", TFunc [VarPar TInt] TVoid),
  (Ident "printStr", TFunc [VarPar TStr] TStr),
  (Ident "readInt", TFunc [] TInt),
  (Ident "readStr", TFunc [] TStr)
  ]

predefinedValues :: [(Ident, Value)]
predefinedValues = [
  (Ident "printInt", funcPrintInt),
  (Ident "printStr", funcPrintStr),
  (Ident "readInt", funcReadInt),
  (Ident "readStr", funcReadStr)
  ]

funcPrintInt :: Value
funcPrintInt = VFunc [AVar parName] f
  where
    parName :: Ident
    parName = Ident "a"
    f :: Executor Value
    f = do
      VInt val <- gets $ getVal parName
      liftIO $ print val
      return VVoid

funcPrintStr :: Value
funcPrintStr = VFunc [AVar parName] f
  where
    parName :: Ident
    parName = Ident "a"
    f :: Executor Value
    f = do
      VStr val <- gets $ getVal parName
      liftIO $ putStr val
      return VVoid

funcReadInt :: Value
funcReadInt = VFunc [] f
  where
    f :: Executor Value
    f = do
      str <- liftIO $ getLine
      case readMaybe str of
        Nothing -> throwError $ runtimeErr $ "could not convert " ++ show str ++ " to int"
        Just v -> return $ VInt v

funcReadStr :: Value
funcReadStr = VFunc [] f
  where
    f :: Executor Value
    f = do
      str <- liftIO $ getLine
      return $ VStr str
