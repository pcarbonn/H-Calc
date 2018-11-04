module Interpreter.B_Add where

  -- this module adds the following language construct to the DSL
  --    (Val α i)
  --    (Add α (i1,i2))
  -------------------------------------------------------

  import Interpreter.A_Nucleus
  import Interpreter.Transfos
  
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

  
  -- syntactic sugar for embedded DSL
  --------------------------------------------------------

  fromInteger :: ('[EmptyNoteF, ValF] :<<: xs) => Integer -> EADT xs
  fromInteger i = Val EmptyNote $ fromIntegral i

  fromRational :: ('[EmptyNoteF, FloatValF] :<<: xs) => Rational -> EADT xs
  fromRational i = FloatVal EmptyNote $ realToFrac i

  (.+) :: ('[EmptyNoteF, AddF] :<<: xs) => EADT xs -> EADT xs -> EADT xs
  (.+) a b = Add EmptyNote (a,b)

  neg :: (ValF :<: xs) => EADT xs -> EADT xs
  neg (Val      α i) = Val      α (-i)


  -- parser
  --------------------------------------------------------

  valParser :: ('[EmptyNoteF, ValF] :<<: xs) => MParser (EADT xs)
  valParser = Val EmptyNote . toInt <$> some M.digitChar
    where toInt :: [Char] -> Int
          toInt cs = foldl' (\a i -> a * 10 + digitToInt i) 0  cs

          digitToInt :: Char -> Int
          digitToInt c = ord c - ord '0'

  floatValParser :: ('[EmptyNoteF, FloatValF] :<<: xs) => MParser (EADT xs)
  floatValParser = FloatVal EmptyNote . toFloat <$> do
          i1 <- some M.digitChar
          _ <- string "."
          i2 <- some M.digitChar
          return (i1, i2)
    where toFloat :: ([Char], [Char]) -> Float
          toFloat (i1,i2) 
            = foldl' (\a i -> a * 10.0 + realToFrac (digitToInt i)) 0.0 i1
            + (foldl' (\a i -> a * 10.0 + realToFrac (digitToInt i)) 0.0 i2)
              / (10.0 ^ (length i2))

          digitToInt :: Char -> Int
          digitToInt c = ord c - ord '0'

  addParser :: ('[EmptyNoteF, AddF] :<<: xs) => MParser (EADT xs) -> MParser (EADT xs)
  addParser termP = Add EmptyNote <$> do
    i1 <- termP
    _ <- string "+"
    i2 <- termP
    return (i1,i2)


  -- Algebra
  --------------------------------------------------------
  
  instance Algebra ValF where
    showAST' (ValF α i) = show i <> α
  
  instance Algebra FloatValF where
      showAST' (FloatValF α f) = show f <> α

  instance Algebra AddF where
    showAST' (AddF α (v1,v2)) = "(" <> v1 <> " + " <> v2 <> ")" <> α -- no recursive call
    
  
        
  -- Isomorphism
  --------------------------------------------------------

  instance ('[EmptyNoteF, TypF, ValF] :<<: xs) 
           => Isomorphism xs ValF where
    getAnnotation (ValF α _) = α
    setType' (ValF α i) = Val (Typ TInt α) i

  
  instance ('[EmptyNoteF, TypF, FloatValF] :<<: xs) 
           => Isomorphism xs FloatValF where
    getAnnotation (FloatValF α _) = α
    setType' (FloatValF α f) = FloatVal (Typ TFloat α) f

  instance ('[HErrorF, EmptyNoteF, TypF, AddF] :<<: xs
           , Functor (VariantF xs), Algebra (VariantF xs)
           , AlgVariantF (Isomorphism xs) (EADT xs) xs, Isomorphism xs (VariantF xs))
           => Isomorphism xs AddF where
    getAnnotation (AddF α _) = α
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


    
