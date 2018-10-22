module Interpreter.C_Mul where

  -- this module adds the following language construct to the DSL
  --    (Mul i1 i2)
  -------------------------------------------------------

  import Interpreter.A_Annotation
  import Interpreter.B_Add
  import Interpreter.Utils
  import Interpreter.Result

  import Haskus.Utils.EADT
  import Data.Functor.Foldable

  --------------------------------------------------------

  data MulF e = MulF Annotation (e, e) deriving (Functor)
  type MulAddValADT = EADT '[HErrorF, ValF,AddF,MulF]

  pattern Mul :: MulF :<: xs => Annotation -> (EADT xs, EADT xs) -> EADT xs
  pattern Mul a is = VF (MulF a is)

  -- show

  instance ShowAST MulF where
    showAST' (MulF _ (u, v)) = "(" <> u <> " * " <> v <> ")" -- no recursive call

  
  -- Type checker
  -- we can multiply anything by a Int, except error
  --------------------------------------------------------

  instance (MulF :<: ys, ShowAST (VariantF ys), Functor (VariantF ys)) => TypeCheck MulF ys where
    typeCheck' (MulF _ ((u,t1), (v,t2))) =
      case (t1,t2) of
        (TError _, _) -> t1
        (_, TError _) -> t2
        (T "Int", _)  -> t1 
        _             -> TError $ "can't multiply `" <> showAST u <> "` whose type is " <> show t1 <>
                                  " with `" <> showAST v <> "` whose type is " <> show t2


  -- apply distribution : a*(b+c) -> (a*b+a*c)
  -- DSL must have HErrorF, AddF, MulF
  --------------------------------------------------------

  -- distribute multiplication over addition if it matches
  distr' :: (HErrorF :<: f, AddF :<: f, MulF :<: f) => EADT f -> Maybe (EADT f)
  distr' (Mul a (i1, (Add b (i2,i3)))) = Just (Add b ((Mul a (i1,i2)), (Mul a (i1,i3))))
  distr' _                 = Nothing

  distr :: (Functor (VariantF f), HErrorF :<: f, AddF :<: f, MulF :<: f) => EADT f -> EADT f
  distr = bottomUpFixed distr'
  

  -- demultiply : n*a -> a+a+... n times
  -- DSL must have ValF, AddF, MulF + Eval
  --------------------------------------------------------

  demultiply' :: (HErrorF :<: f, ValF :<: f, AddF :<: f, MulF :<: f, Eval (VariantF f (EADT f))) 
                => EADT f -> EADT f
  demultiply' (Mul a (n,b)) =
    case (evalAST n, b) of
      (RError e, _) -> HError e
      (_, HError e) -> HError e
      (RInt i, _) ->
        if  | i <  0 -> HError "Error: can't multiply by a negative number"
            | i == 0 -> Val a 0
            | i == 1 -> b
            | otherwise -> Add a (b, demultiply' (Mul a ((Val a $ i-1), b)))

  demultiply' a         = a
  
  demultiply :: (ValF :<: f, HErrorF :<: f, AddF :<: f, MulF :<: f, Eval (VariantF f (EADT f))
                , Functor (VariantF f)) 
                => EADT f -> EADT f
  demultiply = bottomUp demultiply' . distr

  -- Eval
  
  instance EvalAll xs => Eval (MulF (EADT xs)) where
    eval (MulF u v) = RError "target machine cannot multiply"
  