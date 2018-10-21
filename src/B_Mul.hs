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
  -- DSL must have AddF, MulF
  --------------------------------------------------------

  -- distribute multiplication over addition if it matches
  distr' :: (AddF :<: f, MulF :<: f) => EADT f -> Maybe (EADT f)
  distr' (Mul a (Add b c)) = Just (Add (Mul a b) (Mul a c))
  distr' _                 = Nothing

  distr :: (Functor (VariantF f), AddF :<: f, MulF :<: f) => EADT f -> EADT f
  distr = bottomUpFixed distr'
  

  -- demultiply : n*a -> a+a+... n times
  -- DSL must have ValF, AddF, MulF + Eval
  --------------------------------------------------------

  demultiply' :: (ValF :<: f, AddF :<: f, MulF :<: f, Eval (VariantF f (EADT f))) 
                => EADT f -> Maybe (EADT f)
  demultiply' (Mul n a) =
    if  | i <  0 -> error "can't multiply by a negative number"
        | i == 0 -> Just $ Val 0
        | i == 1 -> Just $ a
        | otherwise -> case demultiply' (Mul (Val $ i-1) a) of
                        Just a' -> Just $ Add a a'
                        Nothing -> error "can't reach this point"
    where i = evalEADT n

  demultiply' _         = Nothing
  
  demultiply :: (ValF :<: f, AddF :<: f, MulF :<: f, Eval (VariantF f (EADT f))
                , Functor (VariantF f)) 
                => EADT f -> EADT f
  demultiply = bottomUpFixed demultiply' . distr
   