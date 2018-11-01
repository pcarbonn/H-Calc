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

  (.*) :: (EmptyNoteF :<: xs, MulF :<: xs) => EADT xs -> EADT xs -> EADT xs
  (.*) a b = Mul EmptyNote (a,b)

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
    showAST' (MulF α (i, v)) = "(" <> i <> " * " <> v <> ")" <> α -- no recursive call

  
  -- Type checker
  --------------------------------------------------------
  instance MulF :<: xs => GetAnnotation xs MulF where
    getAnnotation (MulF α _) = α

  instance  (HErrorF :<: xs, EmptyNoteF :<: xs, TypF :<: xs
            , MulF :<: xs
            , AlgVariantF (GetAnnotation xs) (EADT xs) xs, GetAnnotation xs (VariantF xs)
            , Functor (VariantF xs), ShowAST (VariantF xs)) 
            => SetType xs MulF where
    setType' (MulF α (i, v)) =
      case (i, v) of
        (HError _, _) -> i
        (_, HError _) -> v
        _ -> case (getType i, getType v) of
                (Just TInt, Just TInt)   -> Mul (Typ TInt   α) (i,v)
                (Just TInt, Just TFloat) -> Mul (Typ TFloat α) (i,v)
                (Just TFloat, Just TInt) -> Mul (Typ TFloat α) (i,v)
                (Just t1  , Just t2)     -> 
                         HError $ "can't multiply `" <> showAST i <> "` whose type is " <> show t1 <>
                                  " with `" <> showAST v <> "` whose type is " <> show t2
                (_,_) -> HError "Missing type info in multiplication"

    
  -- apply distribution : a*(b+c) -> (a*b+a*c)
  -- DSL must have HErrorF, AddF, MulF
  --------------------------------------------------------

  distribute :: (HErrorF :<: xs, AddF :<: xs, MulF :<: xs, Functor (VariantF xs)) => EADT xs -> EADT xs
  distribute = bottomUpFixed go
    where 
      go (Mul α (i, (Add β (v1,v2)))) = Just (Add β ((Mul α (i,v1)), (Mul α (i,v2))))
      go (Mul α ((Add β (v1,v2)), i)) = Just (Add β ((Mul α (v1,i)), (Mul α (v2,i))))
      go _                            = Nothing



  -- demultiply : n*a -> a+a+... n times
  -- DSL must have ValF, AddF, MulF + Eval
  --------------------------------------------------------

  class Demultiply ys (f :: * -> *) where
    demultiply' :: f (EADT ys) -> EADT ys
    
  instance
    ( AlgVariantF (Demultiply ys) (EADT ys) xs
    ) => Demultiply ys (VariantF xs)
    where
    demultiply' = algVariantF @(Demultiply ys) demultiply'
  
  instance {-# OVERLAPPABLE #-} f :<: ys => Demultiply ys f where
    demultiply' = VF -- keep the other constructors as is
  
  instance {-# OVERLAPPING #-} 
    ( AddF :<: ys, HErrorF :<: ys, ValF :<: ys
    , AlgVariantF ShowAST Text ys, Functor (VariantF ys)
    ) => Demultiply ys MulF where
    demultiply' (MulF α (i,v)) = 
      case (i, v) of
        (HError e, _) -> HError e
        (_, HError e) -> HError e
        (Val _ i', _) ->
          if | i' < 0 -> HError $ "Error: can't multiply by negative number " <> show i'
             | i' == 0 -> Val α 0
             | i' == 1 -> v
             | otherwise -> Add α (v, demultiply' (MulF α ((Val α $ i'-1), v)))
        (_, _) -> HError $ "Can't multiply by " <> showAST i
  
  demultiply :: (Functor (VariantF xs), Demultiply ys (VariantF xs)) => EADT xs -> EADT ys
  demultiply = cata demultiply'



  -- Eval
  --------------------------------------------------------
  
  instance Eval MulF where
    evalAST' (MulF _ _) = RError "target machine cannot multiply"
  