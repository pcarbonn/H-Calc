
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

  showAST :: (Functor (VariantF xs), Algebra (VariantF xs)) => EADT xs -> Text
  showAST = cata showAST'


  -- Isomorphism :: EADT xs -> EADT xs
  -------------------------------------------------------
  class Isomorphism xs (f :: * -> *) where
    getAnnotation :: f (EADT xs) -> EADT xs
    setType' :: f (EADT xs) -> EADT xs

  instance ( AlgVariantF (Isomorphism ys) (EADT ys) xs) 
          => Isomorphism ys (VariantF xs) where
      getAnnotation = algVariantF @(Isomorphism ys) getAnnotation
      setType'      = algVariantF @(Isomorphism ys) setType'

     
  setType :: ( Isomorphism xs (VariantF xs), Functor (VariantF xs))
             => EADT xs -> EADT xs
  setType = cata setType'


  -- Fix of an isomorphism
  -------------------------------------------------------

  -- bottom up traversal that performs an additional bottom up traversal in
  -- the transformed sub-tree when a transformation occurs. 
  bottomUpFixed :: Functor f => (Fix f -> Maybe (Fix f)) -> Fix f -> Fix f
  bottomUpFixed f = f' . Fix . (fmap (bottomUpFixed f)) . unfix
    where
        f' u = case f u of
          Nothing -> u
          Just v  -> bottomUpFixed f v



  -- Tree reduction
  -------------------------------------------------------

  class Reduction ys (f :: * -> *) where
    demultiply' :: f (EADT ys) -> EADT ys
    
  instance ( AlgVariantF (Reduction ys) (EADT ys) xs ) 
           => Reduction ys (VariantF xs) where
    demultiply' = algVariantF @(Reduction ys) demultiply'
  
  instance {-# OVERLAPPABLE #-} f :<: ys => Reduction ys f where
    demultiply' = VF -- by default, keep as is          



  -- HError s
  -------------------------------------------------------

  instance Algebra HErrorF where
    showAST' (HErrorF s) = s
  
  instance (HErrorF :<: xs, EmptyNoteF :<: xs) => Isomorphism xs HErrorF where
    getAnnotation (HErrorF s) = HError s
    setType' _ = EmptyNote

  instance Eval HErrorF where
    evalAST' (HErrorF s) = RError s


  -- EmptyNote
  -------------------------------------------------------
  
  instance Algebra EmptyNoteF where
    showAST' EmptyNoteF = ""

  instance EmptyNoteF :<: xs => Isomorphism xs EmptyNoteF where
    getAnnotation EmptyNoteF = EmptyNote
    setType' _ = EmptyNote

  instance Eval EmptyNoteF where
    evalAST' EmptyNoteF = RError "Can't evaluate annotations"
    