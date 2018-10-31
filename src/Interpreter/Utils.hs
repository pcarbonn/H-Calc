
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

  -- show
  -------------------------------------------------------
  class ShowAST (f :: * -> *) where
    showAST' :: f Text -> Text
    
  instance (AlgVariantF ShowAST Text xs) => ShowAST (VariantF xs) where
    showAST' = algVariantF @ShowAST showAST'

  showAST :: (Functor (VariantF xs), ShowAST (VariantF xs)) => EADT xs -> Text -- type inferred by GHC
  showAST = cata showAST'


  -- Get Annotation
  -------------------------------------------------------
  class GetAnnotation xs (f :: * -> *) where
    getAnnotation' :: f (EADT xs) -> EADT xs

  instance
    ( AlgVariantF (GetAnnotation ys) (EADT ys) xs
    ) => GetAnnotation ys (VariantF xs)
    where
      getAnnotation' = algVariantF @(GetAnnotation ys) getAnnotation'

  getAnnotation :: (Functor (VariantF xs), GetAnnotation xs (VariantF xs)) => EADT xs -> EADT xs
  getAnnotation = cata getAnnotation'


  -- transformations / recursion schemes
  -------------------------------------------------------
  -- aka unfix >>> fmap (bottomUp f) >>> Fix >>> f
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
  data HErrorF e = HErrorF Text deriving (Functor)
  eadtPattern 'HErrorF "HError"

  instance ShowAST HErrorF where
    showAST' (HErrorF s) = s
  
  instance HErrorF :<: xs => GetAnnotation xs HErrorF where
    getAnnotation' (HErrorF s) = HError s

  instance Eval HErrorF where
    evalAST' (HErrorF s) = RError s


  -- EmptyNote
  -------------------------------------------------------

  data EmptyNoteF e = EmptyNoteF deriving (Functor)
  eadtPattern 'EmptyNoteF "EmptyNote"
  
  instance ShowAST EmptyNoteF where
    showAST' EmptyNoteF = ""

  instance EmptyNoteF :<: xs => GetAnnotation xs EmptyNoteF where
    getAnnotation' EmptyNoteF = EmptyNote

  instance Eval EmptyNoteF where
    evalAST' EmptyNoteF = RError "Can't evaluate annotations"