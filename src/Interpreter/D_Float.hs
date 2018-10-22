module Interpreter.D_Float where

  -- this module adds the following language construct to the DSL
  --    (FloatVal f)
  -------------------------------------------------------

  import Interpreter.B_Add
  import Interpreter.C_Mul
  import Interpreter.Utils
  import Interpreter.Result

  import Haskus.Utils.EADT
  import Data.Functor.Foldable

  --------------------------------------------------------

  data FloatValF e = FloatValF Float deriving (Functor)

  --------------------------------------------------------

  pattern FloatVal :: FloatValF :<: xs => Float -> EADT xs
  pattern FloatVal a = VF (FloatValF a)

  --------------------------------------------------------

  instance ShowAST FloatValF where
      showAST' (FloatValF i) = show i
    
  --------------------------------------------------------

  instance TypeCheck FloatValF ys where
    typeCheck' _ = T "Float"
    
  --------------------------------------------------------

  instance Eval (FloatValF e) where
    eval (FloatValF i) = RFloat i
