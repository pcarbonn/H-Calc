module Interpreter.D_Float where

  -- this module adds the following language construct to the DSL
  --    (FloatVal α f)
  -------------------------------------------------------

  import Interpreter.A_TypeCheck
  import Interpreter.Utils
  import Interpreter.Result

  import Haskus.Utils.EADT
  import Haskus.Utils.EADT.TH
  import Text.Megaparsec
  import Text.Megaparsec.Char as M

  --------------------------------------------------------

  data FloatValF e = FloatValF e Float deriving (Functor)
  eadtPattern 'FloatValF "FloatVal"

  fromRational :: (EmptyNoteF :<: xs, FloatValF :<: xs) => Rational -> EADT xs
  fromRational i = FloatVal EmptyNote $ realToFrac i

  
  --------------------------------------------------------

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


  --------------------------------------------------------

  instance ShowAST FloatValF where
      showAST' (FloatValF α f) = show f <> α
    
  --------------------------------------------------------
  
  instance FloatValF :<: xs => GetAnnotation xs FloatValF where
    getAnnotation' (FloatValF α _) = α

  instance (EmptyNoteF :<: xs, FloatValF :<: xs, TypF :<: xs) => SetType xs FloatValF where
    setType' (FloatValF α f) = FloatVal (Typ TFloat α) f
    
  --------------------------------------------------------

  instance Eval FloatValF where
    evalAST' (FloatValF _ f) = RFloat f
