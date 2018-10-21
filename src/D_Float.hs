module D_Float where

  -- this module adds the following language construct to the DSL
  --    (Float f)
  -------------------------------------------------------

  import A_Error
  import B_Add
  import C_Mul
  import Utils

  import Haskus.Utils.EADT
  import Data.Functor.Foldable

  --------------------------------------------------------

  data FloatValF e = FloatValF Float deriving (Functor)

  pattern FloatVal :: FloatValF :<: xs => Float -> EADT xs
  pattern FloatVal a = VF (FloatValF a)

  instance ShowEADT FloatValF where
      showEADT' (FloatValF i) = show i


  instance TypeCheck FloatValF ys where
    typeCheck' _ = T "Float"

