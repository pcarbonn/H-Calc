
module Interpreter.A_TypeCheck where

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
  

  -- show
  --------------------------------------------------------

  instance ShowAST TypF where
    showAST' (TypF t α) = " :: " <> show t <> α



  -- Get Type
  --------------------------------------------------------
  instance TypF :<: xs => GetAnnotation xs TypF where
    getAnnotation (TypF t α) = Typ t α

  getType :: ( TypF :<: xs, EmptyNoteF :<: xs, Functor (VariantF xs)
             , AlgVariantF (GetAnnotation xs) (EADT xs) xs, GetAnnotation xs (VariantF xs)
             ) => EADT xs -> Maybe TType
  getType = go . getAnnotation . unfix
    where go (Typ t _) = Just t
          go EmptyNote = Nothing -- no annotation anymore
          go α = getType $ getAnnotation $ unfix α



  -- Set Type
  --------------------------------------------------------

  instance (EmptyNoteF :<: xs) => SetType xs HErrorF where
    setType' _ = EmptyNote

  instance (EmptyNoteF :<: xs) => SetType xs EmptyNoteF where
    setType' _ = EmptyNote

  instance (TypF :<: xs, EmptyNoteF :<: xs) => SetType xs TypF where
    setType' (TypF _ α) = α -- erase existing type

  -- helpers

  class SetType xs (f :: * -> *) where
    setType' :: f (EADT xs) -> EADT xs

  instance
    ( AlgVariantF (SetType ys) (EADT ys) xs
    ) => SetType ys (VariantF xs)
    where
      setType' = algVariantF @(SetType ys) setType'
     
  setType :: 
    ( SetType xs (VariantF xs)
    , Functor (VariantF xs)
    , TypF :<: xs
    ) => EADT xs -> EADT xs
  setType = cata setType'
     
  
        
  -- eval
  --------------------------------------------------------

  instance Eval TypF where
    evalAST' (TypF _ _) = RError "Can't evaluate annotations"