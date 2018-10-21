module C_Mul where

  -- this module adds the following language construct to the DSL
  --    (Mul i1 i2)
  -------------------------------------------------------

  import A_Error
  import B_Add
  import Utils

  import Haskus.Utils.EADT
  import Data.Functor.Foldable

  --------------------------------------------------------

  data MulF e = MulF e e deriving (Functor)
  type MulAddValADT = EADT '[HErrorF, ValF,AddF,MulF]

  pattern Mul :: MulF :<: xs => EADT xs -> EADT xs -> EADT xs
  pattern Mul a b = VF (MulF a b)

  instance ShowEADT MulF where
    showEADT' (MulF u v) = "(" <> u <> " * " <> v <> ")" -- no recursive call

  instance EvalAll xs => Eval (MulF (EADT xs)) where
    eval (MulF u v) = 
        case (evalEADT u, evalEADT v) of -- implicit recursion
          (Left a, Left b) -> Left (a*b)
          (e, Left b) -> e
          (_, e) -> e


  -- apply distribution : a*(b+c) -> (a*b+a*c)
  -- DSL must have HErrorF, AddF, MulF
  --------------------------------------------------------

  -- distribute multiplication over addition if it matches
  distr' :: (HErrorF :<: f, AddF :<: f, MulF :<: f) => EADT f -> Maybe (EADT f)
  distr' (Mul a (Add b c)) = Just (Add (Mul a b) (Mul a c))
  distr' _                 = Nothing

  distr :: (Functor (VariantF f), HErrorF :<: f, AddF :<: f, MulF :<: f) => EADT f -> EADT f
  distr = bottomUpFixed distr'
  

  -- demultiply : n*a -> a+a+... n times
  -- DSL must have ValF, AddF, MulF + Eval
  --------------------------------------------------------

  demultiply' :: (HErrorF :<: f, ValF :<: f, AddF :<: f, MulF :<: f, Eval (VariantF f (EADT f))) 
                => EADT f -> Maybe (EADT f)
  demultiply' (Mul n a) =
    case (evalEADT n, a) of
      (Right e, _) -> Just $ HError e
      (_, HError e) -> Just $ HError e
      (Left i, a) ->
        if  | i <  0 -> Just $ HError "Error: can't multiply by a negative number"
            | i == 0 -> Just $ Val 0
            | i == 1 -> Just $ a
            | otherwise -> case demultiply' (Mul (Val $ i-1) a) of
                            Just a' -> Just $ Add a a'
                            Nothing -> Just $ HError "can't reach this point"
    where i = evalEADT n

  demultiply' _         = Nothing
  
  demultiply :: (ValF :<: f, HErrorF :<: f, AddF :<: f, MulF :<: f, Eval (VariantF f (EADT f))
                , Functor (VariantF f)) 
                => EADT f -> EADT f
  demultiply = bottomUpFixed demultiply' . distr
   