module Interpreter.A_Annotation where

  import Interpreter.Utils
  
  import Prelude
  import Haskus.Utils.EADT


  data Typ
    = T Text -- the type of the term
    | TError Text   -- the type of an invalid expression with some explanation
    deriving (Show,Eq)  

  data Annotation = Annotation
    { typ :: Typ
    }
  

  emptyAnnot = Annotation {typ= T ""}

  -- helper for type check
  --------------------------------------------------------

  class TypeCheck (f :: * -> *) ys where
    typeCheck' :: f (EADT ys, Typ) -> Typ

  instance TypeCheck (VariantF '[]) ys where
    typeCheck' _ = TError "no implementation of TypeCheck for this type"

  instance (TypeCheck x ys, TypeCheck (VariantF xs) ys)  => TypeCheck (VariantF (x ': xs)) ys where
    typeCheck' v = case popVariantFHead v of
        Right u -> typeCheck' u
        Left  w -> typeCheck' w   


  instance TypeCheck HErrorF ys where
    typeCheck' _ = T "Error"
