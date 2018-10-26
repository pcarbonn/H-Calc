module Interpreter.B_Add where

  -- this module adds the following language construct to the DSL
  --    (Val i)
  --    (Add i1 i2)
  -------------------------------------------------------

  import Interpreter.A_Annotation
  import Interpreter.Utils
  import Interpreter.Result
  
  import Haskus.Utils.EADT
  import Prelude
  
  data ValF e = ValF e Int deriving (Functor)
  data AddF e = AddF e (e, e) deriving (Functor)
  
  type AddValADT = EADT '[EmptyNoteF,ValF,AddF]
  


  -- define patterns, for creation and pattern matching
  
  pattern Val :: ValF :<: xs => EADT xs -> Int -> EADT xs
  pattern Val α i = VF (ValF α i)
  
  pattern Add :: AddF :<: xs => EADT xs -> (EADT xs, EADT xs) -> EADT xs
  pattern Add α is = VF (AddF α is)

  

  -- show
  
  instance ShowAST ValF where
    showAST' (ValF _ i) = show i
  
  instance ShowAST AddF where
    showAST' (AddF _ (u,v)) = "(" <> u <> " + " <> v <> ")" -- no recursive call
    
  
        
  -- Type checker

  instance (EmptyNoteF :<: ys, ValF :<: ys, TypF :<: ys) => TypeAST ValF ys where
    typeAST' (ValF (a,_) i) = Val (Typ TInt a) i
  

  instance (HErrorF :<: ys, EmptyNoteF :<: ys, AddF :<: ys, TypF :<: ys) => TypeAST AddF ys where
    typeAST' (AddF (_,α') ((u,t1), (v,t2))) =
      case (t1,t2) of
        (HError _, _) -> t1
        (_, HError _) -> t2
        _ -> Add (Typ TInt α') (t1,t2) --TODO check type
    

  -- Eval: returns a Int
  
  instance Eval (ValF e) where
    eval (ValF _ i) = RInt i
  
  instance EvalAll xs => Eval (AddF (EADT xs)) where
    eval (AddF _ (u,v)) = 
      case (evalAST u, evalAST v) of -- implicit recursion
        (RInt a, RInt b) -> RInt (a+b)
        (RFloat a, RFloat b) -> RFloat (a+b)
        (RError e, _) -> RError e
        (_, RError e) -> RError e
        _             -> RError $ "Error in eval(AddF)" 
