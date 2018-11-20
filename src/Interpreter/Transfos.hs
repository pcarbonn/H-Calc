
module Interpreter.Transfos where

  import Prelude

  -- -- This modules declares the AST-wide transformations
  -- -------------------------------------------------------


  -- -- Algebra :: EADT xs -> Fixed type
  -- -------------------------------------------------------
  -- class Algebra (f :: * -> *) where
  --   showAST' :: f Text -> Text
  --   -- add more algebra here

  -- instance (AlgVariantF Algebra Text xs) => Algebra (VariantF xs) where
  --   showAST' = algVariantF @Algebra showAST'

  -- showAST :: (Functor (VariantF xs), Algebra (VariantF xs)) => EADT xs -> Text
  -- showAST = cata showAST'


  -- -- Isomorphism :: EADT xs -> EADT xs
  -- -------------------------------------------------------
  -- class Isomorphism xs (f :: * -> *) where
  --   getAnnotation :: f (EADT xs) -> EADT xs
  --   setType' :: f (EADT xs) -> EADT xs
  --   -- add more isomorphisms here

  -- instance ( AlgVariantF (Isomorphism ys) (EADT ys) xs)
  --         => Isomorphism ys (VariantF xs) where
  --     getAnnotation = algVariantF @(Isomorphism ys) getAnnotation
  --     setType'      = algVariantF @(Isomorphism ys) setType'


  -- setType :: ( Isomorphism xs (VariantF xs), Functor (VariantF xs))
  --            => EADT xs -> EADT xs
  -- setType = cata setType'



  -- -- Tree Expansion : EADT xs -> EADT ys
  -- -------------------------------------------------------
  -- -- --> appendEADT @'[newConstructor], followed by isomorphism



  -- -- Tree reduction : EADT xs -> EADT ys
  -- -------------------------------------------------------

  -- -- removeAnnotation

  -- class RemoveAnnotation ys (f :: * -> *) where
  --   removeAnnotation'      :: f (EADT ys) -> EADT ys

  -- instance ( AlgVariantF (RemoveAnnotation ys) (EADT ys) xs )
  --          => RemoveAnnotation ys (VariantF xs) where
  --   removeAnnotation' = algVariantF @(RemoveAnnotation ys) removeAnnotation'

  -- instance {-# OVERLAPPABLE #-} f :<: ys => RemoveAnnotation ys f where
  --   removeAnnotation' = VF -- if f is in result type, keep as is

  -- removeAnnotation :: (Functor (VariantF xs), RemoveAnnotation ys (VariantF xs))
  --     => EADT xs -> EADT ys
  -- removeAnnotation = cata removeAnnotation'
