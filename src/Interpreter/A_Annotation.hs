module Interpreter.A_Annotation where

  -- this module adds the following language construct to the DSL
  --    (EmptyNote)
  --    (Typ t α)
  -------------------------------------------------------

  import Interpreter.Utils
  import Interpreter.Result
  
  import Prelude
  import Haskus.Utils.EADT

  -- EmptyNote

  data EmptyNoteF e = EmptyNoteF deriving (Functor)

  pattern EmptyNote :: EmptyNoteF :<: xs => EADT xs
  pattern EmptyNote = VF EmptyNoteF

  -- Type t α

  data TTyp = TInt | TFloat deriving Show

  data TypF a = TypF TTyp a deriving (Functor)

  pattern Typ :: TypF :<: xs => TTyp -> EADT xs -> EADT xs
  pattern Typ t a = VF (TypF t a)
  

  -- show
  
  instance ShowAST EmptyNoteF where
    showAST' EmptyNoteF = "?"

  instance ShowAST TypF where
    showAST' (TypF t a) = show t

  -- helper for type check
  --------------------------------------------------------

  class TypeAST (f :: * -> *) ys where
    typeAST' :: f (EADT ys, EADT ys) -> EADT ys -- first is original, 2nd is modified

  instance TypeAST (VariantF '[]) ys where
    typeAST' = error "no implementation of Type Check for this type"

  instance (TypeAST x ys, TypeAST (VariantF xs) ys)  => TypeAST (VariantF (x ': xs)) ys where
    typeAST' v = case popVariantFHead v of
        Right u -> typeAST' u
        Left  w -> typeAST' w
     
  typeAST :: 
    ( TypeAST (VariantF xs) xs
    , Functor (VariantF xs)
    , TypF :<: xs
    ) => EADT xs -> EADT xs
  typeAST e = para typeAST' e 

  instance (EmptyNoteF :<: ys) => TypeAST HErrorF ys where
    typeAST' _ = EmptyNote

  instance (EmptyNoteF :<: ys) => TypeAST EmptyNoteF ys where
    typeAST' _ = EmptyNote

  instance (TypF :<: ys, EmptyNoteF :<: ys) => TypeAST TypF ys where
    typeAST' (TypF _ (_, s)) = s -- erase existing type  

  -- eval
  --------------------------------------------------------

  instance Eval (EmptyNoteF e) where
    eval EmptyNoteF = RError "Can't evaluate annotations"

  instance Eval (TypF e) where
    eval (TypF _ _) = RError "Can't evaluate annotations"