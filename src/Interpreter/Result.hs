module Interpreter.Result where
  
  import Haskus.Utils.EADT

  -- type definition of Result
  --------------------------------------------------------

  data Result 
    = RInt Int
    | RFloat Float
    | RError Text
    deriving Show

  -- helper for eval
  -------------------------------------------------------
  class Eval e where
    eval :: e -> Result

  instance Eval (VariantF '[] e) where
    eval u = RError "no implementation of Eval for this type"

  instance (Eval (x e), Eval (VariantF xs e))  => Eval (VariantF (x ': xs) e) where
    eval v = case popVariantFHead v of
        Right u -> eval u
        Left  w -> eval w

  type EvalAll xs = Eval (VariantF xs (EADT xs))

  evalAST :: EvalAll xs => EADT xs -> Result
  evalAST = eval . unfix  