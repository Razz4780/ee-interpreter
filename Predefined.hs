module Predefined where

import Control.Monad.Except
import Text.Read (readMaybe)
import Errors
import State

funcPrintInt :: ParsedFunc
funcPrintInt ((VInt i):_) = do
  liftIO $ print i
  return VVoid

funcPrintStr :: ParsedFunc
funcPrintStr ((VStr s):_) = do
  liftIO $ putStr s
  return VVoid

funcReadInt :: ParsedFunc
funcReadInt _ = do
  str <- liftIO $ getLine
  case readMaybe str of
    Nothing -> throwError $ runtimeErr $ "could not convert " ++ show str ++ " to int"
    Just v -> return $ VInt v

funcReadStr :: ParsedFunc
funcReadStr _ = do
  str <- liftIO $ getLine
  return $ VStr str
