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

  instance ShowAST (VariantF '[]) where
    showAST' = error "no implementation of Show for this type"

  instance (AlgVariantF ShowAST Text xs) => ShowAST (VariantF xs) where
    showAST' = algVariantF @ShowAST showAST'

  showAST :: ( Functor (VariantF xs), ShowAST (VariantF xs)) => EADT xs -> Text -- type inferred by GHC
  showAST = cata showAST'



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

  instance Eval (HErrorF e) where
    eval (HErrorF s) = RError s
  


  -- EmptyNote
  -------------------------------------------------------

  data EmptyNoteF e = EmptyNoteF deriving (Functor)
  eadtPattern 'EmptyNoteF "EmptyNote"
  
  instance ShowAST EmptyNoteF where
    showAST' EmptyNoteF = ""

  instance Eval (EmptyNoteF e) where
    eval EmptyNoteF = RError "Can't evaluate annotations"