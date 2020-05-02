module Main where

import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Except
import System.IO (getContents)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import qualified Grammar.LexGrammar as LG
import qualified Grammar.ParGrammar as PaG
import qualified Grammar.SkelGrammar as SG
import qualified Grammar.PrintGrammar as PrG
import qualified Grammar.AbsGrammar as AG
import qualified Grammar.ErrM as EG

import qualified Errors
import qualified Static
import qualified Executor
import qualified Predefined
import qualified State

type ParseFun = [LG.Token] -> EG.Err AG.Prog

run :: ParseFun -> String -> IO ()
run p s = let ts = PaG.myLexer s in case p ts of
  EG.Bad err -> do
    putStrLn "\nParse              Failed...\n"
    putStrLn err
    exitFailure
  EG.Ok (AG.Prog ss) -> do
    let e = Static.initEnv Predefined.predefinedTypes
    let checkRes = runExcept $ runStateT (Static.stmtChecker $ AG.SBlock $ AG.Block ss) e
    case checkRes of
      Left err -> do
        Errors.printError err
        exitFailure
      Right _ -> do
        let s = State.initState Predefined.predefinedValues
        exRes <- runExceptT $ runStateT (Executor.stmtExecutor $ AG.SBlock $ AG.Block ss) s
        case exRes of
          Left err -> do
            Errors.printError err
            exitFailure
          Right _ -> exitSuccess

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (filename)      Read file."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    (fn:_) -> readFile fn >>= run PaG.pProg
    _ -> usage
