module Interpreter.C_Mul where

  -- this module adds the following language construct to the DSL
  --    (Mul α (i1,i2))
  -------------------------------------------------------

  import Interpreter.A_Nucleus
  import Interpreter.B_Add
  import Interpreter.Transfos

  import Haskus.Utils.EADT
  import Haskus.Utils.EADT.TH
  import Text.Megaparsec.Char as M

  --------------------------------------------------------

  data MulF e = MulF e (e, e) deriving (Functor)
  eadtPattern 'MulF "Mul"

  (.*) :: ('[EmptyNoteF, MulF] :<<: xs) => EADT xs -> EADT xs -> EADT xs
  (.*) a b = Mul EmptyNote (a,b)

  -- parser
  --------------------------------------------------------

  mulParser :: ('[EmptyNoteF, MulF] :<<: xs) => MParser (EADT xs) -> MParser (EADT xs)
  mulParser factorP = Mul EmptyNote <$> do
    i1 <- factorP
    _ <- string "*"
    i2 <- factorP
    return (i1,i2)
    
  -- Algebra
  --------------------------------------------------------

  instance Algebra MulF where
    showAST' (MulF α (i, v)) = "(" <> i <> " * " <> v <> ")" <> α -- no recursive call

  
  -- Isomorphism
  --------------------------------------------------------
  instance ('[HErrorF, EmptyNoteF, TypF, MulF] :<<: xs
           , AlgVariantF (Isomorphism xs) (EADT xs) xs, Isomorphism xs (VariantF xs)
           , Functor (VariantF xs), Algebra (VariantF xs))
          => Isomorphism xs MulF where
    getAnnotation (MulF α _) = α
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

  distribute :: ('[HErrorF, AddF, MulF] :<<: xs, Functor (VariantF xs)) => EADT xs -> EADT xs
  distribute = bottomUpFixed go
    where 
      go (Mul α (i, (Add β (v1,v2)))) = Just (Add β ((Mul α (i,v1)), (Mul α (i,v2))))
      go (Mul α ((Add β (v1,v2)), i)) = Just (Add β ((Mul α (v1,i)), (Mul α (v2,i))))
      go _                            = Nothing



  -- demultiply : n*a -> a+a+... n times
  -- DSL must have ValF, AddF, MulF + Eval
  --------------------------------------------------------
  
  instance {-# OVERLAPPING #-} 
            ( '[AddF, HErrorF, ValF] :<<: ys
            , AlgVariantF Algebra Text ys, Functor (VariantF ys) )
            => Demultiply ys MulF where
    demultiply' (MulF α (v1,v2)) = 
      case (v1, v2) of
        (HError e, _) -> HError e
        (_, HError e) -> HError e
        (Val _ v1', _) -> go v1' v2
        (_, Val _ v2') -> go v2' v1
        (_, _) -> HError $ "Can't multiply by " <> showAST v1
      where go i v = 
              if | i < 0 -> HError $ "Error: can't multiply by negative number " <> show i
                 | i == 0 -> Val α 0
                 | i == 1 -> v
                 | otherwise -> Add α (v, demultiply' (MulF α ((Val α $ i-1), v)))        
  
  