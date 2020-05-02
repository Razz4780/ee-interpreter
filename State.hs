module State where

import qualified Data.Map as M
import Data.Maybe (fromJust)
import Control.Monad.State
import Control.Monad.Except
import Grammar.AbsGrammar
import Errors

type Executor a = StateT ProgState (ExceptT Err IO) a

data AInfo = AVar Ident | AVal Ident

data Value = VInt Integer | VBool Bool | VStr String | VVoid | VFunc [AInfo] (Executor Value)

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

putVars :: VarMap -> ProgState -> ProgState
putVars vr (ProgState _ vl nxt) = ProgState vr vl nxt

linkNames :: Ident -> Ident -> ProgState -> ProgState
linkNames n1 n2 (ProgState vr vl nxt) =
  let l = fromJust $ M.lookup n2 vr
  in ProgState (M.insert n1 l vr) vl nxt
