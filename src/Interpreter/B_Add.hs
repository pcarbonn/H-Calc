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

  instance GetType ValF where
    getType' (ValF a _) = a  

  instance GetType AddF where
    getType' (AddF a _) = a


  instance (EmptyNoteF :<: ys, ValF :<: ys, TypF :<: ys, GetType (VariantF ys), Functor (VariantF ys)) => SetType ValF ys where
    setType' (ValF (a,_) i) = Val (Typ TInt a) i

  instance (HErrorF :<: ys, EmptyNoteF :<: ys, AddF :<: ys, TypF :<: ys, 
            GetType (VariantF ys), Functor (VariantF ys), ShowAST (VariantF ys)) 
            => SetType AddF ys where
    setType' (AddF (_,α') ((u,u'), (v,v'))) =
      case (u',v') of
        (HError _, _) -> u'
        (_, HError _) -> v'
        _ -> case (getType u', getType v') of
                (TInt, TInt) -> Add (Typ TInt α') (u',v')
                (TFloat, TFloat) -> Add (Typ TFloat α') (u',v')
                (t1,t2) -> HError $ "can't add `" <> showAST u' <> "` whose type is " <> show t1 <>
                                    " with `" <> showAST v' <> "` whose type is " <> show t2
    


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
