module Interpreter.B_Add where

  -- this module adds the following language construct to the DSL
  --    (Val α i)
  --    (Add α (i1,i2))
  -------------------------------------------------------

  import Interpreter.A_Nucleus
  import Interpreter.Transfos

  import Fmt
  import Haskus.Utils.EADT
  import Haskus.Utils.EADT.TH
  import Text.Megaparsec
  import Text.Megaparsec.Char as M
  import Text.Show


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

  neg :: ('[HErrorF, EmptyNoteF, ValF] :<<: xs, Functor (VariantF xs), AlgVariantF Algebra Text xs)
        => EADT xs -> EADT xs
  neg (Val      α i) = Val      α (-i)
  neg v = HError EmptyNote $ format "can't negate {}" (showAST v)


  -- parser
  --------------------------------------------------------

  valParser :: ('[EmptyNoteF, ValF] :<<: xs) => MParser (EADT xs)
  valParser = Val EmptyNote . toInt <$> do
        s <- option "+" (string "-")
        i <- some M.digitChar
        _ <- option () spaceConsumer
        return (s,i)
    where toInt :: (Text, [Char]) -> Int
          toInt (s, cs) = s' * (foldl' (\a i -> a * 10 + digitToInt i) 0  cs)
            where s' = if s == "+" then 1 else -1

          digitToInt :: Char -> Int
          digitToInt c = ord c - ord '0'

  floatValParser :: ('[EmptyNoteF, FloatValF] :<<: xs) => MParser (EADT xs)
  floatValParser = FloatVal EmptyNote . toFloat <$> do
          s <- option "+" (string "-")
          i1 <- some M.digitChar
          _ <- string "."
          i2 <- some M.digitChar
          _ <- option () spaceConsumer
          return (s, i1, i2)
    where toFloat :: (Text, [Char], [Char]) -> Float
          toFloat (s, i1,i2)
            = s' * (foldl' (\a i -> a * 10.0 + realToFrac (digitToInt i)) 0.0 i1
                   + (foldl' (\a i -> a * 10.0 + realToFrac (digitToInt i)) 0.0 i2)
                   / (10.0 ^ (length i2))
                  )
              where s' = if s == "+" then 1 else -1

          digitToInt :: Char -> Int
          digitToInt c = ord c - ord '0'

  addParser :: ('[EmptyNoteF, AddF] :<<: xs) => MParser (EADT xs) -> MParser (EADT xs)
  addParser termP = Add EmptyNote <$> do
    i1 <- termP
    _ <- symbol "+"
    i2 <- termP
    return (i1,i2)


  -- Algebra
  --------------------------------------------------------

  instance Algebra ValF where
    showAST' (ValF α i) = format "{}{}" i α

  instance Algebra FloatValF where
      showAST' (FloatValF α f) = format "{}{}" f α

  instance Algebra AddF where
    showAST' (AddF α (v1,v2)) = format "({} + {}){}" v1 v2 α -- no recursive call



  -- Isomorphism
  --------------------------------------------------------

  instance ('[TypF, ValF] :<<: xs)
           => Isomorphism xs ValF where
    getAnnotation (ValF α _) = α
    setType' (ValF α i) = Val (Typ α TInt) i


  instance ('[TypF, FloatValF] :<<: xs)
           => Isomorphism xs FloatValF where
    getAnnotation (FloatValF α _) = α
    setType' (FloatValF α f) = FloatVal (Typ α TFloat) f

  instance ('[HErrorF, EmptyNoteF, TypF, AddF] :<<: xs
           , Functor (VariantF xs), Algebra (VariantF xs)
           , AlgVariantF (Isomorphism xs) (EADT xs) xs, Isomorphism xs (VariantF xs))
           => Isomorphism xs AddF where
    getAnnotation (AddF α _) = α
    setType' (AddF α (v1, v2)) =
      case (v1,v2) of
        (HError _ _, _) -> v1
        (_, HError _ _) -> v2
        _ -> case (getType v1, getType v2) of
                (Just TInt  , Just TInt  ) -> Add (Typ α TInt) (v1,v2)
                (Just TFloat, Just TFloat) -> Add (Typ α TFloat) (v1,v2)
                (Just t1    , Just t2    ) ->
                         HError α $ format "can't add `{}` whose type is {} with `{}` whose type is "
                                    (showAST v1) (show t1) (showAST v2) (show t2)
                (_,_) -> HError α "Missing type info in addition"
