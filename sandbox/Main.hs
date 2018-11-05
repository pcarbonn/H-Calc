
module Main where

  import Interpreter.Interpreter
  import Prelude

  -- Main

  main :: IO ()
  main = do
    putTextLn $ show $ interpret "((2+1)*5.0)"
