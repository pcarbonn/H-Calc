module Interpreter.B_Add where

  -- this module adds the following language construct to the DSL
  --    (Val i)
  --    (Add i1 i2)
  -------------------------------------------------------

  import Interpreter.A_Annotation
  import Interpreter.Utils
  import Interpreter.Result
  
  import Haskus.Utils.EADT
  import Data.Functor.Foldable
  
  data ValF e = ValF Annotation Int deriving (Functor)
  data AddF e = AddF Annotation (e, e) deriving (Functor)
  
  type AddValADT = EADT '[ValF,AddF]
  


  -- define patterns, for creation and pattern matching
  
  pattern Val :: ValF :<: xs => Annotation -> Int -> EADT xs
  pattern Val α i = VF (ValF α i)
  
  pattern Add :: AddF :<: xs => Annotation -> (EADT xs, EADT xs) -> EADT xs
  pattern Add α is = VF (AddF α is)

  

  -- show
  
  instance ShowAST ValF where
    showAST' (ValF _ i) = show i
  
  instance ShowAST AddF where
    showAST' (AddF _ (u,v)) = "(" <> u <> " + " <> v <> ")" -- no recursive call
    
  
        
  -- Type checker
  
  instance TypeCheck ValF ys where
    typeCheck' _ = T "Int"

  instance (AddF :<: ys, ShowAST (VariantF ys), Functor (VariantF ys)) => TypeCheck AddF ys where
    typeCheck' (AddF _ ((u,t1), (v,t2)))
      | t1 == t2       = t1
      | TError _ <- t1 = t1 -- propagate errors
      | TError _ <- t2 = t2
      | otherwise = TError $ "can't add `" <> showAST u <> "` whose type is " <> show t1 <>
                            " with `" <> showAST v <> "` whose type is " <> show t2
  
  

  -- Eval: returns a Int
  
  instance Eval (ValF e) where
    eval (ValF _ i) = RInt i
  
  instance EvalAll xs => Eval (AddF (EADT xs)) where
    eval (AddF _ (u,v)) = 
      case (evalAST u, evalAST v) of -- implicit recursion
        (RInt a, RInt b) -> RInt (a+b)
        (RError e, _) -> RError e
        (_, RError e) -> RError e
        _             -> RError $ "Error in eval(AddF)" 
