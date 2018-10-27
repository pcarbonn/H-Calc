module Interpreter.D_Float where

  -- this module adds the following language construct to the DSL
  --    (FloatVal α f)
  -------------------------------------------------------

  import Interpreter.A_TypeCheck
  import Interpreter.B_Add
  import Interpreter.C_Mul
  import Interpreter.Utils
  import Interpreter.Result

  import Haskus.Utils.EADT
  import Text.Megaparsec
  import Text.Megaparsec.Char as M

  --------------------------------------------------------

  data FloatValF e = FloatValF e Float deriving (Functor)

  pattern FloatVal :: FloatValF :<: xs => EADT xs -> Float -> EADT xs
  pattern FloatVal α f = VF (FloatValF α f)


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

  instance GetType FloatValF where
    getType' (FloatValF α _) = α  

  instance (EmptyNoteF :<: xs, FloatValF :<: xs, TTypeF :<: xs) => SetType FloatValF xs where
    setType' (FloatValF α f) = FloatVal (Typ TFloat α) f
    
  --------------------------------------------------------

  instance Eval (FloatValF e) where
    eval (FloatValF _ f) = RFloat f
