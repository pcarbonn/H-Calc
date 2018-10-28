module Interpreter.C_Mul where

  -- this module adds the following language construct to the DSL
  --    (Mul α (i1,i2))
  -------------------------------------------------------

  import Interpreter.A_TypeCheck
  import Interpreter.B_Add
  import Interpreter.Utils
  import Interpreter.Result

  import Haskus.Utils.EADT
  import Haskus.Utils.EADT.TH
  import Text.Megaparsec
  import Text.Megaparsec.Char as M

  --------------------------------------------------------

  data MulF e = MulF e (e, e) deriving (Functor)
  eadtPattern 'MulF "Mul"


  -- parser
  --------------------------------------------------------

  mulParser :: (EmptyNoteF :<: xs, MulF :<: xs) => MParser (EADT xs) -> MParser (EADT xs)
  mulParser factorP = Mul EmptyNote <$> do
    i1 <- factorP
    _ <- string "*"
    i2 <- factorP
    return (i1,i2)
    
  -- show
  --------------------------------------------------------

  instance ShowAST MulF where
    showAST' (MulF _ (i, v)) = "(" <> i <> " * " <> v <> ")" -- no recursive call

  
  -- Type checker
  --------------------------------------------------------

  instance GetType MulF where
    getType' (MulF α _) = α

  instance  (HErrorF :<: xs, EmptyNoteF :<: xs, TTypeF :<: xs
            , MulF :<: xs
            , GetType (VariantF xs), Functor (VariantF xs), ShowAST (VariantF xs)) 
            => SetType MulF xs where
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

  distribute :: (HErrorF :<: xs, AddF :<: xs, MulF :<: xs, Functor (VariantF xs)) => EADT xs -> EADT xs
  distribute = bottomUpFixed go
    where 
      go (Mul α (i, (Add β (v1,v2)))) = Just (Add β ((Mul α (i,v1)), (Mul α (i,v2))))
      go _                            = Nothing



  -- demultiply : n*a -> a+a+... n times
  -- DSL must have ValF, AddF, MulF + Eval
  --------------------------------------------------------

  demultiply :: (HErrorF :<: xs, ValF :<: xs, AddF :<: xs, MulF :<: xs
                , Eval (VariantF xs (EADT xs)), Functor (VariantF xs)) 
                => EADT xs -> EADT xs
  demultiply = bottomUp go
    where go (Mul α (i,v)) =
            case (evalAST i, v) of
              (RError e, _) -> HError e
              (_, HError e) -> HError e
              (RInt i', _) ->
                if  | i' <  0 -> HError $ "Error: can't multiply by negative number " <> show i'
                    | i' == 0 -> Val α 0
                    | i' == 1 -> v
                    | otherwise -> Add α (v, go (Mul α ((Val α $ i'-1), v)))

          go a         = a


  -- Eval
  --------------------------------------------------------
  
  instance EvalAll xs => Eval (MulF (EADT xs)) where
    eval (MulF _ _) = RError "target machine cannot multiply"
  