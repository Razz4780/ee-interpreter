module State where

import qualified Data.Map as M
import Data.Maybe (fromJust)
import Control.Monad.State
import Control.Monad.Except
import Grammar.AbsGrammar
import Errors

type Executor = StateT ProgState (ExceptT Err IO) Value

type ParsedFunc = [Value] -> Executor

data Value = VInt Integer | VBool Bool | VStr String | VVoid | VFunc ParsedFunc

type VarMap = M.Map Ident Int

type ValMap = M.Map Int Value

data ProgState = ProgState {
    vars :: VarMap,
    vals :: ValMap,
    next :: Int
}

getVal :: Ident -> ProgState -> Value
getVal n (ProgState vr vl _) =
  let l = fromJust $ M.lookup n vr
  in fromJust $ M.lookup l vl

insertVal :: Ident -> Value -> ProgState -> ProgState
insertVal n v (ProgState vr vl nxt) =
  let vr' = M.insert n nxt vr
      vl' = M.insert nxt v vl
  in ProgState vr' vl' $ nxt + 1

assVal :: Ident -> Value -> ProgState -> ProgState
assVal n v (ProgState vr vl nxt) = 
  let loc = fromJust $ M.lookup n vr
  in ProgState vr (M.insert loc v vl) nxt
