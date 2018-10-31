module Interpreter.Result where
  
  import Haskus.Utils.EADT
  import Prelude

  -- type definition of Result
  --------------------------------------------------------

  data Result 
    = RInt Int
    | RFloat Float
    | RError Text
    deriving (Show, Eq)

  -- helper for evalAST
  -------------------------------------------------------
  
  class Eval (f :: * -> *) where
    evalAST' :: f Result -> Result

  instance AlgVariantF Eval Result xs => Eval (VariantF xs) where
    evalAST' = algVariantF @Eval evalAST'

  evalAST :: (Functor (VariantF xs), Eval (VariantF xs)) => EADT xs -> Result
  evalAST = cata evalAST'  