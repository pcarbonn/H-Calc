module A_Error where

  -- this module defines the primitive language construct of the DSL
  --    (HError text)
  -------------------------------------------------------

  import Utils
  
  import Haskus.Utils.EADT
  import Data.Functor.Foldable
  
  data HErrorF e = HErrorF Text deriving (Functor)


  -- define pattern, for creation and pattern matching
  
  pattern HError :: HErrorF :<: xs => Text -> EADT xs
  pattern HError a = VF (HErrorF a)

  

  -- show
  
  instance ShowAST HErrorF where
    showAST' (HErrorF s) = s

    

  -- Type checker
  
  instance TypeCheck HErrorF ys where
    typeCheck' _ = T "Error"
  
  

  -- Eval: returns itself
  
  instance Eval (HErrorF e) where
    eval (HErrorF e) = Right e

  