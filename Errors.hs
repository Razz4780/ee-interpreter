module Errors where

import           Grammar.PrintGrammar

data Err = Err [String] String

addContext :: Print a => a -> Err -> Err
addContext node (Err code msg) = Err ((printTree node):code) msg

printError :: Err -> IO ()
printError (Err code msg) = do
  putStrLn "An error occurred!"
  mapM_ printContext code
  putStrLn $ "<--- " ++ msg
  where
    printContext :: String -> IO ()
    printContext context = do
      putStrLn "Inside code:"
      putStrLn context

runtimeErr :: String -> Err
runtimeErr msg = Err [] $ "runtime error: " ++ msg

staticErr :: String -> Err
staticErr msg = Err [] $ "static check error: " ++ msg
