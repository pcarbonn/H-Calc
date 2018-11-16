module Interpreter.C_Mul where

  -- this module adds the following language construct to the DSL
  --    (Mul α (i1,i2))
  -------------------------------------------------------

  import Interpreter.A_Nucleus
  import Interpreter.B_Add
  import Interpreter.Transfos

  import Fmt
  import Text.Megaparsec.Char as M
  import Text.Show


  -- define nodes
  --------------------------------------------------------


  -- parser
  --------------------------------------------------------


  -- Algebra
  --------------------------------------------------------


  -- Isomorphism
  --------------------------------------------------------


  -- apply distribution : a*(b+c) -> (a*b+a*c)
  --------------------------------------------------------



  -- demultiply : n*a -> a+a+... n times
  --------------------------------------------------------
