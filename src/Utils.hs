module Utils where

  import Control.Arrow
  import Haskus.Utils.EADT

  -- this module defines the operations we want to perform on the AST
  --    showEADT
  --    simplify
  --    typeCheck
  --    evalEADT
  -------------------------------------------------------

  -- show
  -------------------------------------------------------
  class ShowEADT (f :: * -> *) where
    showEADT' :: f Text -> Text

  instance ShowEADT (VariantF '[]) where
    showEADT' = error "no implementation of Show for this type"

  instance (ShowEADT x, ShowEADT (VariantF xs))  => ShowEADT (VariantF (x ': xs)) where
    showEADT' v = case popVariantFHead v of
        Right u -> showEADT' u
        Left  w -> showEADT' w
    
  showEADT :: (ShowEADT (Base t), Recursive t) => t -> Text -- type inferred by GHC
  showEADT = cata showEADT'


  -- fix of transformation
  -------------------------------------------------------
  -- bottom up traversal that performs an additional bottom up traversal in
  -- the transformed sub-tree when a transformation occurs. 
  bottomUpFixed :: Functor f => (Fix f -> Maybe (Fix f)) -> Fix f -> Fix f
  bottomUpFixed f = unfix >>> fmap (bottomUpFixed f) >>> Fix >>> f'
    where
        f' u = case f u of
          Nothing -> u
          Just v  -> bottomUpFixed f v

{-   -- simplify = remove feature from the set
  -------------------------------------------------------
  class Simplify (f :: * -> *) ys where
    simplify :: f (EADT ys) -> EADT ys

  -- boilerplate

  instance Simplify (VariantF '[]) ys where
    simplify = error "no implementation of Simplify for this type"

  instance (Simplify x ys, Simplify (VariantF xs) ys)  => Simplify (VariantF (x ': xs)) ys where
    simplify v = case popVariantFHead v of
        Right u -> simplify u
        Left  w -> simplify w -}

  -- Type check
  --------------------------------------------------------

  data Typ
    = T Text -- the type of the term
    | TError Text   -- the type of an invalid expression with some explanation
    deriving (Show,Eq)

  class TypeCheck (f :: * -> *) ys where
    typeCheck' :: f (EADT ys, Typ) -> Typ

  instance TypeCheck (VariantF '[]) ys where
    typeCheck' = error "no implementation of TypeCheck for this type"

  instance (TypeCheck x ys, TypeCheck (VariantF xs) ys)  => TypeCheck (VariantF (x ': xs)) ys where
    typeCheck' v = case popVariantFHead v of
        Right u -> typeCheck' u
        Left  w -> typeCheck' w   
              
  -- generic eval
  -------------------------------------------------------
  class Eval e where
    eval :: e -> Either Int Text

  instance Eval (VariantF '[] e) where
    eval u = Right "no implementation of Eval for this type"

  instance (Eval (x e), Eval (VariantF xs e))  => Eval (VariantF (x ': xs) e) where
    eval v = case popVariantFHead v of
        Right u -> eval u
        Left  w -> eval w

  type EvalAll xs = Eval (VariantF xs (EADT xs))

  evalEADT :: EvalAll xs => EADT xs -> Either Int Text
  evalEADT = eval . unfix