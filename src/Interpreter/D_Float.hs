module Interpreter.D_Float where

  -- this module adds the following language construct to the DSL
  --    (FloatVal f)
  -------------------------------------------------------

  import Interpreter.A_Annotation
  import Interpreter.B_Add
  import Interpreter.C_Mul
  import Interpreter.Utils
  import Interpreter.Result

  import Haskus.Utils.EADT
  import Prelude

  --------------------------------------------------------

  data FloatValF e = FloatValF Annotation Float deriving (Functor)

  --------------------------------------------------------

  pattern FloatVal :: FloatValF :<: xs => Annotation -> Float -> EADT xs
  pattern FloatVal α f = VF (FloatValF α f)

  --------------------------------------------------------

  instance ShowAST FloatValF where
      showAST' (FloatValF _ i) = show i
    
  --------------------------------------------------------

  instance TypeCheck FloatValF ys where
    typeCheck' _ = T "Float"
    
  --------------------------------------------------------

  instance Eval (FloatValF e) where
    eval (FloatValF _ i) = RFloat i
