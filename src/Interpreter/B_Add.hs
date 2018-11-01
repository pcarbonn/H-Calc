module Interpreter.B_Add where

  -- this module adds the following language construct to the DSL
  --    (Val α i)
  --    (Add α (i1,i2))
  -------------------------------------------------------

  import Interpreter.A_TypeCheck
  import Interpreter.Utils
  import Interpreter.Result
  
  import Haskus.Utils.EADT
  import Haskus.Utils.EADT.TH
  import Text.Megaparsec
  import Text.Megaparsec.Char as M
  

  -- define nodes
  --------------------------------------------------------

  data ValF      e =      ValF e Int   deriving (Functor)
  data FloatValF e = FloatValF e Float deriving (Functor)
  data AddF      e = AddF e (e, e)     deriving (Functor)

  -- define patterns, for creation and pattern matching
  
  eadtPattern 'ValF      "Val"
  eadtPattern 'FloatValF "FloatVal"
  eadtPattern 'AddF      "Add"

  fromInteger :: (EmptyNoteF :<: xs, ValF :<: xs) => Integer -> EADT xs
  fromInteger i = Val EmptyNote $ fromIntegral i

  fromRational :: (EmptyNoteF :<: xs, FloatValF :<: xs) => Rational -> EADT xs
  fromRational i = FloatVal EmptyNote $ realToFrac i

  (.+) :: (EmptyNoteF :<: xs, AddF :<: xs) => EADT xs -> EADT xs -> EADT xs
  (.+) a b = Add EmptyNote (a,b)

  neg :: ValF :<: xs => EADT xs -> EADT xs
  neg (Val α i)= Val α (-i)

  -- parser
  --------------------------------------------------------

  valParser :: (EmptyNoteF :<: xs, ValF :<: xs) => MParser (EADT xs)
  valParser = Val EmptyNote . toInt <$> some M.digitChar
    where toInt :: [Char] -> Int
          toInt cs = foldl' (\a i -> a * 10 + digitToInt i) 0  cs

          digitToInt :: Char -> Int
          digitToInt c = ord c - ord '0'

  floatValParser :: (EmptyNoteF :<: xs, FloatValF :<: xs) => MParser (EADT xs)
  floatValParser = FloatVal EmptyNote . toFloat <$> do
          i1 <- some M.digitChar
          _ <- string "."
          i2 <- some M.digitChar
          return (i1, i2)
    where toFloat :: ([Char], [Char]) -> Float
          toFloat (i1,i2) = foldl' (\a i -> a * 10.0 + realToFrac (digitToInt i)) 0.0 i1

          digitToInt :: Char -> Int
          digitToInt c = ord c - ord '0'

  addParser :: (EmptyNoteF :<: xs, AddF :<: xs) => MParser (EADT xs) -> MParser (EADT xs)
  addParser termP = Add EmptyNote <$> do
    i1 <- termP
    _ <- string "+"
    i2 <- termP
    return (i1,i2)


  -- show
  --------------------------------------------------------
  
  instance ShowAST ValF where
    showAST' (ValF α i) = show i <> α
  
  instance ShowAST FloatValF where
      showAST' (FloatValF α f) = show f <> α

  instance ShowAST AddF where
    showAST' (AddF α (v1,v2)) = "(" <> v1 <> " + " <> v2 <> ")" <> α -- no recursive call
    
  
        
  -- Type checker
  --------------------------------------------------------

  instance ValF :<: xs => GetAnnotation xs ValF where
    getAnnotation (ValF α _) = α

  
  instance FloatValF :<: xs => GetAnnotation xs FloatValF where
    getAnnotation (FloatValF α _) = α

  instance AddF :<: xs => GetAnnotation xs AddF where
    getAnnotation (AddF α _) = α



  instance (EmptyNoteF :<: xs, ValF :<: xs, TypF :<: xs, GetAnnotation xs (VariantF xs), Functor (VariantF xs)) 
          => SetType xs ValF where
    setType' (ValF α i) = Val (Typ TInt α) i

  instance (EmptyNoteF :<: xs, FloatValF :<: xs, TypF :<: xs) => SetType xs FloatValF where
    setType' (FloatValF α f) = FloatVal (Typ TFloat α) f

  instance (HErrorF :<: xs, EmptyNoteF :<: xs, TypF :<: xs
           , AddF :<: xs
           , AlgVariantF (GetAnnotation xs) (EADT xs) xs, GetAnnotation xs (VariantF xs)
           , Functor (VariantF xs), ShowAST (VariantF xs)) 
            => SetType xs AddF where
    setType' (AddF α (v1, v2)) =
      case (v1,v2) of
        (HError _, _) -> v1
        (_, HError _) -> v2
        _ -> case (getType v1, getType v2) of
                (Just TInt  , Just TInt  ) -> Add (Typ TInt α) (v1,v2)
                (Just TFloat, Just TFloat) -> Add (Typ TFloat α) (v1,v2)
                (Just t1    , Just t2    ) -> 
                         HError $ "can't add `" <> showAST v1 <> "` whose type is " <> show t1 <>
                                  " with `" <> showAST v2 <> "` whose type is " <> show t2
                (_,_) -> HError "Missing type info in addition"
    


  -- Eval: returns a Int
  --------------------------------------------------------
  
  instance Eval ValF where
    evalAST' (ValF _ i) = RInt i

  instance Eval FloatValF where
    evalAST' (FloatValF _ f) = RFloat f  
    
  instance Eval AddF where
    evalAST' (AddF _ (v1,v2)) = 
      case (v1, v2) of -- implicit recursion
        (RInt v1', RInt v2') -> RInt (v1'+v2')
        (RFloat v1', RFloat v2') -> RFloat (v1'+v2')
        (RError e, _) -> RError e
        (_, RError e) -> RError e
        (a,b)             -> RError $ "Error in eval(" <> show a <> "+" <> show b <> ")"
