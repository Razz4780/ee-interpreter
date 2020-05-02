module Predefined where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map             as M
import           Errors
import           Executor
import           Grammar.AbsGrammar
import           State
import           Text.Read            (readMaybe)

predefinedTypes :: [(Ident, Type)]
predefinedTypes =
  [ (Ident "printInt", TFunc [ValPar TInt] TVoid)
  , (Ident "printStr", TFunc [ValPar TStr] TVoid)
  , (Ident "readInt", TFunc [] TInt)
  , (Ident "readStr", TFunc [] TStr)
  ]

predefinedValues :: [(Ident, Value)]
predefinedValues =
  [ (Ident "printInt", funcPrintInt)
  , (Ident "printStr", funcPrintStr)
  , (Ident "readInt", funcReadInt)
  , (Ident "readStr", funcReadStr)
  ]

funcPrintInt :: Value
funcPrintInt = VFunc f
  where
    f :: [Expr] -> Executor Value
    f (e:_) = do
      VInt val <- exprExecutor e
      liftIO $ putStr $ show val
      return VVoid

funcPrintStr :: Value
funcPrintStr = VFunc f
  where
    f :: [Expr] -> Executor Value
    f (e:_) = do
      VStr val <- exprExecutor e
      liftIO $ putStr val
      return VVoid

funcReadInt :: Value
funcReadInt = VFunc f
  where
    f :: [Expr] -> Executor Value
    f _ = do
      str <- liftIO $ getLine
      case readMaybe str of
        Nothing -> throwError $ runtimeErr $ "could not convert " ++ show str ++ " to int"
        Just v -> return $ VInt v

funcReadStr :: Value
funcReadStr = VFunc f
  where
    f :: [Expr] -> Executor Value
    f _ = do
      str <- liftIO $ getLine
      return $ VStr str
