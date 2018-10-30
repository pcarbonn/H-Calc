
module Interpreter.A_TypeCheck where

  -- this module adds the following annotation construct to the DSL
  --    (Typ t α)
  -------------------------------------------------------

  import Interpreter.Utils
  import Interpreter.Result
  
  import Haskus.Utils.EADT
  import Haskus.Utils.EADT.TH

  -- Type t α

  data TType = TInt | TFloat deriving Show

  data TypF e = TypF TType e deriving (Functor)
  eadtPattern 'TypF "Typ"
  

  -- show
  --------------------------------------------------------

  instance ShowAST TypF where
    showAST' (TypF t α) = " :: " <> show t <> α



  -- Get Type
  --------------------------------------------------------
  
  instance GetType HErrorF where
    getType' _ = error "no type in annotation"

  instance GetType EmptyNoteF where
    getType' _ = error "no type in annotation"

  instance GetType TypF where
    getType' (TypF t _)  = t
    
  
  -- helpers
  
  class GetType (f :: * -> *) where
    getType' :: f TType -> TType    

  instance GetType (VariantF '[]) where
    getType' = error "no implementation of Type Check for this type"

  instance (AlgVariantF GetType TType xs) => GetType (VariantF xs) where
    getType' = algVariantF @GetType getType'

  getType :: ( Functor (VariantF xs), GetType (VariantF xs)) => EADT xs -> TType
  getType = cata getType'



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

  instance SetType xs (VariantF '[]) where
    setType' = error "no implementation of Type Check for this type"

  instance (SetType ys x, SetType ys (VariantF xs))  => SetType ys (VariantF (x ': xs)) where
    setType' v = case popVariantFHead v of
        Right u -> setType' u
        Left  w -> setType' w
     
  setType :: 
    ( SetType xs (VariantF xs)
    , Functor (VariantF xs)
    , TypF :<: xs
    ) => EADT xs -> EADT xs
  setType = cata setType'
        
        
  -- eval
  --------------------------------------------------------

  instance Eval (TypF e) where
    evalAST' (TypF _ _) = RError "Can't evaluate annotations"