module A_Add where

  -- this module adds the following language construct to the DSL
  --    (Val i)
  --    (Add i1 i2)
  -- when showing an expression, the order of operands is reversed
  -------------------------------------------------------

  import Utils
  
  import Haskus.Utils.EADT
  import Data.Functor.Foldable
  
  data ValF e = ValF Int deriving (Functor)
  data AddF e = AddF e e deriving (Functor)
  
  type AddValADT = EADT '[ValF,AddF]
  
  -- define patterns, for creation and pattern matching
  
  pattern Val :: ValF :<: xs => Int -> EADT xs
  pattern Val a = VF (ValF a)
  
  pattern Add :: AddF :<: xs => EADT xs -> EADT xs -> EADT xs
  pattern Add a b = VF (AddF a b)

  
  -- show
  
  instance ShowEADT ValF where
    showEADT' (ValF i) = show i
  
  instance ShowEADT AddF where
    showEADT' (AddF u v) = "(" <> v <> " + " <> u <> ")" -- no recursive call
  
  showEADT :: (ShowEADT (Base t), Recursive t) => t -> Text -- type inferred by GHC
  showEADT = cata showEADT'
  
  
  -- Eval: returns an int
  
  instance Eval (ValF e) where
    eval (ValF i) = i
  
  instance EvalAll xs => Eval (AddF (EADT xs)) where
    eval (AddF u v) = evalEADT u + evalEADT v -- implicit recursion
    
  
  