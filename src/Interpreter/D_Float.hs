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

  data FloatValF e = FloatValF e Float deriving (Functor)

  --------------------------------------------------------

  pattern FloatVal :: FloatValF :<: xs => EADT xs -> Float -> EADT xs
  pattern FloatVal α f = VF (FloatValF α f)

  --------------------------------------------------------

  instance ShowAST FloatValF where
      showAST' (FloatValF _ i) = show i
    
  --------------------------------------------------------

  instance GetType FloatValF where
    getType' (FloatValF a _) = a  

  instance (EmptyNoteF :<: ys, FloatValF :<: ys, TypF :<: ys) => SetType FloatValF ys where
    setType' (FloatValF a i) = FloatVal (Typ TFloat a) i
    
  --------------------------------------------------------

  instance Eval (FloatValF e) where
    eval (FloatValF _ i) = RFloat i
