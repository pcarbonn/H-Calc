module Utils where

  import Result
  import Control.Arrow
  import Haskus.Utils.EADT

  -- this module defines the operations we want to perform on the AST
  --    showAST
  --    typeCheck
  --    simplify
  -------------------------------------------------------
  data HErrorF e = HErrorF Text deriving (Functor)


  -- define pattern, for creation and pattern matching
  
  pattern HError :: HErrorF :<: xs => Text -> EADT xs
  pattern HError a = VF (HErrorF a)

  
  -- show
  -------------------------------------------------------
  class ShowAST (f :: * -> *) where
    showAST' :: f Text -> Text

  instance ShowAST (VariantF '[]) where
    showAST' = error "no implementation of Show for this type"

  instance (ShowAST x, ShowAST (VariantF xs))  => ShowAST (VariantF (x ': xs)) where
    showAST' v = case popVariantFHead v of
        Right u -> showAST' u
        Left  w -> showAST' w
    
  showAST :: (ShowAST (Base t), Recursive t) => t -> Text -- type inferred by GHC
  showAST = cata showAST'

  instance ShowAST HErrorF where
    showAST' (HErrorF s) = s
    

  -- Type check
  --------------------------------------------------------

  data Typ
    = T Text -- the type of the term
    | TError Text   -- the type of an invalid expression with some explanation
    deriving (Show,Eq)

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

              
  -- eval
  -------------------------------------------------------

  instance Eval (HErrorF e) where
    eval (HErrorF e) = RError e
