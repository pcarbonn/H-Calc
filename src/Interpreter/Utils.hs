
module Interpreter.Utils where
  
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


  -- Algebra :: EADT xs -> Fixed type
  -------------------------------------------------------
  class Algebra (f :: * -> *) where
    showAST' :: f Text -> Text
    -- add more algebra here
    
  instance (AlgVariantF Algebra Text xs) => Algebra (VariantF xs) where
    showAST' = algVariantF @Algebra showAST'

  showAST :: (Functor (VariantF xs), Algebra (VariantF xs)) => EADT xs -> Text
  showAST = cata showAST'


  -- Isomorphism :: EADT xs -> EADT xs
  -------------------------------------------------------
  class Isomorphism xs (f :: * -> *) where
    getAnnotation :: f (EADT xs) -> EADT xs
    setType' :: f (EADT xs) -> EADT xs
    -- add more isomorphism here

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



  -- Tree reduction : EADT xs -> EADT ys
  -------------------------------------------------------
  -- there must be one class for each output type, ys

  -- removeAnnotation

  class RemoveAnnotation ys (f :: * -> *) where
    removeAnnotation'      :: f (EADT ys) -> EADT ys
    
  instance ( AlgVariantF (RemoveAnnotation ys) (EADT ys) xs ) 
           => RemoveAnnotation ys (VariantF xs) where
    removeAnnotation' = algVariantF @(RemoveAnnotation ys) removeAnnotation'  
  
  instance {-# OVERLAPPABLE #-} f :<: ys => RemoveAnnotation ys f where
    removeAnnotation' = VF -- if f is in result type, keep as is      

  removeAnnotation :: (Functor (VariantF xs), RemoveAnnotation ys (VariantF xs)) 
      => EADT xs -> EADT ys
  removeAnnotation = cata removeAnnotation'


  -- Demultiply

  class Demultiply ys (f :: * -> *) where
    demultiply'      :: f (EADT ys) -> EADT ys
    
  instance ( AlgVariantF (Demultiply ys) (EADT ys) xs ) 
           => Demultiply ys (VariantF xs) where
    demultiply' = algVariantF @(Demultiply ys) demultiply'
  
  instance {-# OVERLAPPABLE #-} f :<: ys => Demultiply ys f where
    demultiply' = VF -- if f is in result type, keep as is          

  demultiply :: (Functor (VariantF xs), Demultiply ys (VariantF xs)) 
                => EADT xs -> EADT ys
  demultiply = cata demultiply'


  
  -- EmptyNote
  -------------------------------------------------------
  
  data EmptyNoteF e = EmptyNoteF deriving (Functor)
  eadtPattern 'EmptyNoteF "EmptyNote"

  instance Algebra EmptyNoteF where
    showAST' EmptyNoteF = ""

  instance EmptyNoteF :<: xs => Isomorphism xs EmptyNoteF where
    getAnnotation EmptyNoteF = EmptyNote
    setType' _ = EmptyNote
    

  -- HError s
  -------------------------------------------------------

  data HErrorF e = HErrorF Text deriving (Functor)
  eadtPattern 'HErrorF "HError"

  instance Algebra HErrorF where
    showAST' (HErrorF s) = s
  
  instance (HErrorF :<: xs, EmptyNoteF :<: xs) => Isomorphism xs HErrorF where
    getAnnotation (HErrorF s) = HError s
    setType' _ = EmptyNote

