
module Interpreter.A_Nucleus where

  -- this module defines the following nodes
  --    (HError Text)
  --    (EmptyNote)
  --    (Typ t α)
  -------------------------------------------------------

  import Interpreter.Transfos
  
  import Haskus.Utils.EADT
  import Haskus.Utils.EADT.TH

  
  -- AST nodes
  -------------------------------------------------------
  
  data EmptyNoteF e = EmptyNoteF deriving (Functor)
  eadtPattern 'EmptyNoteF "EmptyNote"

  data HErrorF e = HErrorF Text deriving (Functor)
  eadtPattern 'HErrorF "HError"

  data TType = TInt | TFloat deriving (Show, Eq)

  data TypF e = TypF TType e deriving (Functor)
  eadtPattern 'TypF "Typ"
  

  -- Transformations
  --------------------------------------------------------

  -- algebra

  instance Algebra HErrorF where
    showAST' (HErrorF s) = s

  instance Algebra EmptyNoteF where
    showAST' EmptyNoteF = ""

  instance Algebra TypF where
    showAST' (TypF t α) = " :: " <> show t <> α
  
  -- isomorphism

  instance (HErrorF :<: xs, EmptyNoteF :<: xs) => Isomorphism xs HErrorF where
    getAnnotation (HErrorF s) = HError s
    setType' _ = EmptyNote

  instance TypF :<: xs => Isomorphism xs TypF where
    getAnnotation (TypF t α) = Typ t α
    setType' (TypF _ α) = α -- erase existing type

  instance EmptyNoteF :<: xs => Isomorphism xs EmptyNoteF where
    getAnnotation EmptyNoteF = EmptyNote
    setType' _ = EmptyNote

  -- other

  getType :: ( TypF :<: xs, EmptyNoteF :<: xs, Functor (VariantF xs)
             , AlgVariantF (Isomorphism xs) (EADT xs) xs, Isomorphism xs (VariantF xs)
             ) => EADT xs -> Maybe TType
  getType = go . getAnnotation . unfix
    where go (Typ t _) = Just t
          go EmptyNote = Nothing -- no annotation anymore
          go α = getType $ getAnnotation $ unfix α


  instance (EmptyNoteF :<: xs) => RemoveAnnotation xs TypF where
    removeAnnotation' (TypF t α) = EmptyNote