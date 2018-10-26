
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

  data TypF e = TypF TTyp e deriving (Functor)

  pattern Typ :: TypF :<: xs => TTyp -> EADT xs -> EADT xs
  pattern Typ t a = VF (TypF t a)
  

  -- show
  
  instance ShowAST EmptyNoteF where
    showAST' EmptyNoteF = "?"

  instance ShowAST TypF where
    showAST' (TypF t α) = " :: " <> show t <> α



  -- Get Type
  --------------------------------------------------------
  class GetType (f :: * -> *) where
    getType' :: f TTyp -> TTyp    

  instance GetType (VariantF '[]) where
    getType' = error "no implementation of Type Check for this type"

  instance (GetType x, GetType (VariantF xs))  => GetType (VariantF (x ': xs)) where
    getType' v = case popVariantFHead v of
        Right u -> getType' u
        Left  w -> getType' w

  getType :: (GetType (Base t), Recursive t) => t -> TTyp
  getType = cata getType'
  
  instance GetType HErrorF where
    getType' _ = error "no type in annotation"

  instance GetType EmptyNoteF where
    getType' _ = error "no type in annotation"

  instance GetType TypF where
    getType' (TypF t _)  = t



  -- Set Type
  --------------------------------------------------------
  class SetType (f :: * -> *) ys where
    setType' :: f (EADT ys) -> EADT ys

  instance SetType (VariantF '[]) ys where
    setType' = error "no implementation of Type Check for this type"

  instance (SetType x ys, SetType (VariantF xs) ys)  => SetType (VariantF (x ': xs)) ys where
    setType' v = case popVariantFHead v of
        Right u -> setType' u
        Left  w -> setType' w
     
  setType :: 
    ( SetType (VariantF xs) xs
    , Functor (VariantF xs)
    , TypF :<: xs
    ) => EADT xs -> EADT xs
  setType = cata setType'


  instance (EmptyNoteF :<: ys) => SetType HErrorF ys where
    setType' _ = EmptyNote

  instance (EmptyNoteF :<: ys) => SetType EmptyNoteF ys where
    setType' _ = EmptyNote

  instance (TypF :<: ys, EmptyNoteF :<: ys) => SetType TypF ys where
    setType' (TypF _ α) = α -- erase existing type


        
        
  -- eval
  --------------------------------------------------------

  instance Eval (EmptyNoteF e) where
    eval EmptyNoteF = RError "Can't evaluate annotations"

  instance Eval (TypF e) where
    eval (TypF _ _) = RError "Can't evaluate annotations"