module Interpreter.B_Add where

  -- this module adds the following language construct to the DSL
  --    (Val α i)
  --    (Add α (i1,i2))
  -------------------------------------------------------

  import Interpreter.A_Annotation
  import Interpreter.Utils
  import Interpreter.Result
  
  import Haskus.Utils.EADT
  import Prelude
  
  data ValF e = ValF e Int deriving (Functor)
  data AddF e = AddF e (e, e) deriving (Functor)
  

  -- define patterns, for creation and pattern matching
  
  pattern Val :: ValF :<: xs => EADT xs -> Int -> EADT xs
  pattern Val α i = VF (ValF α i)
  
  pattern Add :: AddF :<: xs => EADT xs -> (EADT xs, EADT xs) -> EADT xs
  pattern Add α is = VF (AddF α is)

  

  -- show
  
  instance ShowAST ValF where
    showAST' (ValF _ i) = show i
  
  instance ShowAST AddF where
    showAST' (AddF _ (v1,v2)) = "(" <> v1 <> " + " <> v2 <> ")" -- no recursive call
    
  
        
  -- Type checker

  instance GetType ValF where
    getType' (ValF α _) = α  

  instance GetType AddF where
    getType' (AddF α _) = α


  instance (EmptyNoteF :<: ys, ValF :<: ys, TTypeF :<: ys, GetType (VariantF ys), Functor (VariantF ys)) => SetType ValF ys where
    setType' (ValF α i) = Val (Typ TInt α) i

  instance (HErrorF :<: ys, EmptyNoteF :<: ys, TTypeF :<: ys
           , AddF :<: ys
           , GetType (VariantF ys), Functor (VariantF ys), ShowAST (VariantF ys)) 
            => SetType AddF ys where
    setType' (AddF α (v1, v2)) =
      case (v1,v2) of
        (HError _, _) -> v1
        (_, HError _) -> v2
        _ -> case (getType v1, getType v2) of
                (TInt, TInt) -> Add (Typ TInt α) (v1,v2)
                (TFloat, TFloat) -> Add (Typ TFloat α) (v1,v2)
                (t1,t2) -> HError $ "can't add `" <> showAST v1 <> "` whose type is " <> show t1 <>
                                    " with `" <> showAST v2 <> "` whose type is " <> show t2
    


  -- Eval: returns a Int
  
  instance Eval (ValF e) where
    eval (ValF _ i) = RInt i
  
  instance EvalAll xs => Eval (AddF (EADT xs)) where
    eval (AddF _ (v1,v2)) = 
      case (evalAST v1, evalAST v2) of -- implicit recursion
        (RInt v1', RInt v2') -> RInt (v1'+v2')
        (RFloat v1', RFloat v2') -> RFloat (v1'+v2')
        (RError e, _) -> RError e
        (_, RError e) -> RError e
        _             -> RError $ "Error in eval(AddF)" 