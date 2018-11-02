
module Interpreter.Utils where

  import Interpreter.Result
  
  import Haskus.Utils.EADT
  import Haskus.Utils.EADT.TH
  import Prelude

  -- This modules defines the following AST node:
  --    (HError Text)
  --    (EmptyNote)
  -- Additionally, it creates helpers for the operations we want to perform on the AST
  --    showAST
  --    simplify
  -------------------------------------------------------

  -- AST nodes
  -------------------------------------------------------

  data HErrorF e = HErrorF Text deriving (Functor)
  eadtPattern 'HErrorF "HError"


  data EmptyNoteF e = EmptyNoteF deriving (Functor)
  eadtPattern 'EmptyNoteF "EmptyNote"

  -- Algebra :: EADT xs -> Fixed type
  -------------------------------------------------------
  class Algebra (f :: * -> *) where
    showAST' :: f Text -> Text
    
  instance (AlgVariantF Algebra Text xs) => Algebra (VariantF xs) where
    showAST' = algVariantF @Algebra showAST'

  showAST :: (Functor (VariantF xs), Algebra (VariantF xs)) => EADT xs -> Text -- type inferred by GHC
  showAST = cata showAST'


  -- Isomorphism :: EADT xs -> EADT xs
  -------------------------------------------------------
  class Isomorphism xs (f :: * -> *) where
    getAnnotation :: f (EADT xs) -> EADT xs
  
  instance
      ( Isomorphism xs f
      , Isomorphism xs (VariantF fs)
      ) => Isomorphism xs (VariantF (f ': fs)) where
    getAnnotation v =  case popVariantFHead v of
            Right u -> getAnnotation u
            Left  w -> getAnnotation w  

  instance HErrorF :<: xs => Isomorphism xs (VariantF '[]) where
    getAnnotation _ = HError "can't GetAnnotation of empty tree"  


  -- bottom-up transformations
  -------------------------------------------------------
  -- aka unfix >>> fmap (bottomUp f) >>> Fix >>> f
  bottomUp :: Functor f => (Fix f -> Fix f) -> Fix f -> Fix f
  bottomUp f = f . Fix . (fmap (bottomUp f)) . unfix

  -- bottom up traversal that performs an additional bottom up traversal in
  -- the transformed sub-tree when a transformation occurs. 
  bottomUpFixed :: Functor f => (Fix f -> Maybe (Fix f)) -> Fix f -> Fix f
  bottomUpFixed f = f' . Fix . (fmap (bottomUpFixed f)) . unfix
    where
        f' u = case f u of
          Nothing -> u
          Just v  -> bottomUpFixed f v


  -- HError s
  -------------------------------------------------------

  instance Algebra HErrorF where
    showAST' (HErrorF s) = s
  
  instance HErrorF :<: xs => Isomorphism xs HErrorF where
    getAnnotation (HErrorF s) = HError s

  instance Eval HErrorF where
    evalAST' (HErrorF s) = RError s


  -- EmptyNote
  -------------------------------------------------------
  
  instance Algebra EmptyNoteF where
    showAST' EmptyNoteF = ""

  instance EmptyNoteF :<: xs => Isomorphism xs EmptyNoteF where
    getAnnotation EmptyNoteF = EmptyNote

  instance Eval EmptyNoteF where
    evalAST' EmptyNoteF = RError "Can't evaluate annotations"
    