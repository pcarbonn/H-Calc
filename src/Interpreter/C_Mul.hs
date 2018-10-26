module Interpreter.C_Mul where

  -- this module adds the following language construct to the DSL
  --    (Mul α (i1,i2))
  -------------------------------------------------------

  import Interpreter.A_Annotation
  import Interpreter.B_Add
  import Interpreter.Utils
  import Interpreter.Result

  import Haskus.Utils.EADT
  import Prelude

  --------------------------------------------------------

  data MulF e = MulF e (e, e) deriving (Functor)

  pattern Mul :: MulF :<: xs => EADT xs -> (EADT xs, EADT xs) -> EADT xs
  pattern Mul α is = VF (MulF α is)

  -- show

  instance ShowAST MulF where
    showAST' (MulF _ (i, v)) = "(" <> i <> " * " <> v <> ")" -- no recursive call

  
  -- Type checker
  --------------------------------------------------------

  instance GetType MulF where
    getType' (MulF α _) = α

  instance  (HErrorF :<: ys, EmptyNoteF :<: ys, TypF :<: ys
            , MulF :<: ys
            , GetType (VariantF ys), Functor (VariantF ys), ShowAST (VariantF ys)) 
            => SetType MulF ys where
    setType' (MulF α (i, v)) =
      case (i, v) of
        (HError _, _) -> i
        (_, HError _) -> v
        _ -> case (getType i, getType v) of
                (TInt, TInt) -> Mul (Typ TInt α) (i,v)
                (TInt, TFloat) -> Mul (Typ TFloat α) (i,v)
                (t1,t2) -> HError $ "can't multiply `" <> showAST i <> "` whose type is " <> show t1 <>
                                    " with `" <> showAST v <> "` whose type is " <> show t2

    
  -- apply distribution : a*(b+c) -> (a*b+a*c)
  -- DSL must have HErrorF, AddF, MulF
  --------------------------------------------------------

  -- distribute multiplication over addition if it matches
  distr' :: (HErrorF :<: xs, AddF :<: xs, MulF :<: xs) => EADT xs -> Maybe (EADT xs)
  distr' (Mul α (i, (Add β (v1,v2)))) = Just (Add β ((Mul α (i,v1)), (Mul α (i,v2))))
  distr' _                 = Nothing

  distr :: (HErrorF :<: xs, AddF :<: xs, MulF :<: xs, Functor (VariantF xs)) => EADT xs -> EADT xs
  distr = bottomUpFixed distr'
  

  -- demultiply : n*a -> a+a+... n times
  -- DSL must have ValF, AddF, MulF + Eval
  --------------------------------------------------------

  demultiply :: (HErrorF :<: xs, ValF :<: xs, AddF :<: xs, MulF :<: xs
                , Eval (VariantF xs (EADT xs)), Functor (VariantF xs)) 
                => EADT xs -> EADT xs
  demultiply = bottomUp go . distr
    where go (Mul α (i,v)) =
            case (evalAST i, v) of
              (RError e, _) -> HError e
              (_, HError e) -> HError e
              (RInt i', _) ->
                if  | i' <  0 -> HError "Error: can't multiply by a negative number"
                    | i' == 0 -> Val α 0
                    | i' == 1 -> i
                    | otherwise -> Add α (v, go (Mul α ((Val α $ i'-1), v)))

          go a         = a

  -- Eval
  
  instance EvalAll xs => Eval (MulF (EADT xs)) where
    eval (MulF _ _) = RError "target machine cannot multiply"
  