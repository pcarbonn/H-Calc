
module Interpreter.Transfos where
  
  import Haskus.Utils.EADT
  import Haskus.Utils.EADT.TH
  import Prelude

  -- This modules declares the AST-wide transformations
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


  -- Tree Expansion : EADT xs -> EADT ys
  -------------------------------------------------------
  -- appendEADT @'[newConstructor], followed by isomorphism


  
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




