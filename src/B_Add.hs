module B_Add where

  -- this module adds the following language construct to the DSL
  --    (Val i)
  --    (Add i1 i2)
  -------------------------------------------------------

  import A_Error
  import Utils
  
  import Haskus.Utils.EADT
  import Data.Functor.Foldable
  
  data ValF e = ValF Int deriving (Functor)
  data AddF e = AddF e e deriving (Functor)
  
  type AddValADT = EADT '[ValF,AddF]
  


  -- define patterns, for creation and pattern matching
  
  pattern Val :: ValF :<: xs => Int -> EADT xs
  pattern Val a = VF (ValF a)
  
  pattern Add :: AddF :<: xs => EADT xs -> EADT xs -> EADT xs
  pattern Add a b = VF (AddF a b)

  

  -- show
  
  instance ShowEADT ValF where
    showEADT' (ValF i) = show i
  
  instance ShowEADT AddF where
    showEADT' (AddF u v) = "(" <> u <> " + " <> v <> ")" -- no recursive call
  
  

  -- Eval: returns an int
  
  instance Eval (ValF e) where
    eval (ValF i) = Left i
  
  instance EvalAll xs => Eval (AddF (EADT xs)) where
    eval (AddF u v) = 
      case (evalEADT u, evalEADT v) of -- implicit recursion
        (Left a, Left b) -> Left (a+b)
        (e, Left b) -> e
        (_, e) -> e
    
  
        
  -- Type checker
  
  instance TypeCheck ValF ys where
    typeCheck' _ = T "Int"

  instance (AddF :<: ys, ShowEADT (VariantF ys), Functor (VariantF ys)) => TypeCheck AddF ys where
    typeCheck' (AddF (u,t1) (v,t2))
      | t1 == t2       = t1
      | TError _ <- t1 = t1 -- propagate errors
      | TError _ <- t2 = t2
      | otherwise = TError $ "can't add `" <> showEADT u <> "` whose type is " <> show t1 <>
                            " with `" <> showEADT v <> "` whose type is " <> show t2
  