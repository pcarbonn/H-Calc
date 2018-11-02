
module Interpreter.A_Type where

  -- this module adds the following annotation construct to the DSL
  --    (Typ t α)
  -------------------------------------------------------

  import Interpreter.Utils
  import Interpreter.Result
  
  import Haskus.Utils.EADT
  import Haskus.Utils.EADT.TH

  -- Type t α

  data TType = TInt | TFloat deriving (Show, Eq)

  data TypF e = TypF TType e deriving (Functor)
  eadtPattern 'TypF "Typ"
  

  -- Transformations
  --------------------------------------------------------

  instance Algebra TypF where
    showAST' (TypF t α) = " :: " <> show t <> α

  instance TypF :<: xs => Isomorphism xs TypF where
    getAnnotation (TypF t α) = Typ t α
    setType' (TypF _ α) = α -- erase existing type


  getType :: ( TypF :<: xs, EmptyNoteF :<: xs, Functor (VariantF xs)
             , AlgVariantF (Isomorphism xs) (EADT xs) xs, Isomorphism xs (VariantF xs)
             ) => EADT xs -> Maybe TType
  getType = go . getAnnotation . unfix
    where go (Typ t _) = Just t
          go EmptyNote = Nothing -- no annotation anymore
          go α = getType $ getAnnotation $ unfix α


        
  -- eval
  --------------------------------------------------------

  instance Eval TypF where
    evalAST' (TypF _ _) = RError "Can't evaluate annotations"