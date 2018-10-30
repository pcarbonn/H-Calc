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
  class Eval e where
    evalAST' :: e -> Result

  instance Eval (VariantF '[] e) where
    evalAST' u = RError "no implementation of Eval for this type"

  instance (Eval (x e), Eval (VariantF xs e))  => Eval (VariantF (x ': xs) e) where
    evalAST' v = case popVariantFHead v of
        Right u -> evalAST' u
        Left  w -> evalAST' w

  type EvalAll xs = Eval (VariantF xs (EADT xs))

  evalAST :: EvalAll xs => EADT xs -> Result
  evalAST = evalAST' . unfix  