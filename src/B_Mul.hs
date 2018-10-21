module B_Mul where

  -- this module adds the following language construct to the DSL
  --    (Mul i1 i2)
  -------------------------------------------------------

  import A_Add
  import Utils

  import Haskus.Utils.EADT
  import Data.Functor.Foldable

  --------------------------------------------------------

  data MulF e = MulF e e deriving (Functor)
  type MulAddValADT = EADT '[ValF,AddF,MulF]

  pattern Mul :: MulF :<: xs => EADT xs -> EADT xs -> EADT xs
  pattern Mul a b = VF (MulF a b)

  instance ShowEADT MulF where
    showEADT' (MulF u v) = "(" <> u <> " * " <> v <> ")" -- no recursive call

  instance EvalAll xs => Eval (MulF (EADT xs)) where
    eval (MulF u v) = evalEADT u * evalEADT v -- explicit recursion  


  -- apply distribution : a*(b+c) -> (a*b+a*c)
  --------------------------------------------------------

  -- distribute multiplication over addition if it matches
  distr' :: (AddF :<: f, MulF :<: f) => EADT f -> Maybe (EADT f)
  distr' (Mul a (Add b c)) = Just (Add (Mul a b) (Mul a c))
  distr' _                 = Nothing

  distr :: (Functor (VariantF f), AddF :<: f, MulF :<: f) => EADT f -> EADT f
  distr = bottomUpFixed distr'
  
  
  -- simplify multiplication a*b -> a+b
  -- (it makes no sense but it's just an example)
  --------------------------------------------------------

  instance{-# OVERLAPPING #-} AddF :<: ys => Simplify MulF ys where
    simplify (MulF u v) = Add u v

  instance {-# OVERLAPPABLE #-} f :<: ys => Simplify f ys where
    simplify = VF  -- the other constructors are kept as-is

