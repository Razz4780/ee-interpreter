module Errors where

import Grammar.PrintGrammar

data Err = Err [String] String

addContext :: Print a => a -> Err -> Err
addContext node (Err code msg) = Err ((printTree node):code) msg

printErrors :: Err -> IO ()
printErrors (Err code msg) = do
  putStrLn "An error occurred!"
  mapM_ printContext code
  putStrLn $ "<--- " ++ msg
  where
    printContext :: String -> IO ()
    printContext context = do
      putStrLn "Inside code:"
      putStrLn context
