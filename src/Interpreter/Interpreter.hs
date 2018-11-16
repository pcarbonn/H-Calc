module Interpreter.Interpreter where

  -- this module is the main entry point of the interpreter

  import Interpreter.A_Nucleus
  import Interpreter.B_Add
  import Interpreter.C_Mul
  import Interpreter.Transfos

  import Fmt
  import Text.Megaparsec
  import Text.Megaparsec.Char as M
  import Text.Show

  -- main parser
  --------------------------------------------------------




  -- evaluation
  --------------------------------------------------------

  data Result
    = RInt Int
    | RFloat Float
    | RError Text
    deriving (Show, Eq)



  -- type specialisation
  --------------------------------------------------------



  -- interpret
  --------------------------------------------------------
