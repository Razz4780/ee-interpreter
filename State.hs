module State where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map             as M
import           Data.Maybe           (fromJust)
import           Errors
import           Grammar.AbsGrammar

type Executor a = StateT ProgState (ExceptT Err IO) a

data Value = VInt Integer | VBool Bool | VStr String | VVoid | VFunc ([Expr] -> Executor Value)

type VarMap = M.Map Ident Int

type ValMap = M.Map Int Value

data ProgState = ProgState {
    vars :: VarMap,
    vals :: ValMap,
    next :: Int
}

getLoc :: Ident -> ProgState -> Int
getLoc n (ProgState vr _ _) = fromJust $ M.lookup n vr

getVal :: Ident -> ProgState -> Value
getVal n s@(ProgState _ vl _) = fromJust $ M.lookup (getLoc n s) vl

defVal :: Ident -> Value -> ProgState -> ProgState
defVal n v (ProgState vr vl nxt) =
  let vr' = M.insert n nxt vr
      vl' = M.insert nxt v vl
  in ProgState vr' vl' $ nxt + 1

assVal :: Ident -> Value -> ProgState -> ProgState
assVal n v s@(ProgState vr vl nxt) = ProgState vr (M.insert (getLoc n s) v vl) nxt

initState :: [(Ident, Value)] -> ProgState
initState v = foldr insertPair (ProgState M.empty M.empty 0) v
  where
    insertPair :: (Ident, Value) -> ProgState -> ProgState
    insertPair (n, v) (ProgState vr vl nxt) =
      ProgState (M.insert n nxt vr) (M.insert nxt v vl) $ nxt + 1

putVars :: VarMap -> ProgState -> ProgState
putVars vr (ProgState _ vl nxt) = ProgState vr vl nxt

scopeVars :: Executor a -> Executor a
scopeVars e = do
  vr <- gets vars
  res <- e
  modify $ putVars vr
  return res
