module Interpreter.C_Mul where

  -- this module adds the following language construct to the DSL
  --    (Mul α (i1,i2))
  -------------------------------------------------------

  import Interpreter.A_Nucleus
  import Interpreter.B_Add
  import Interpreter.Transfos

  import Haskus.Utils.ContFlow
  import Haskus.Utils.EADT
  import Haskus.Utils.EADT.TH
  import Text.Megaparsec.Char as M

  --------------------------------------------------------

  data MulF e = MulF e (e, e) deriving (Functor)
  eadtPattern 'MulF "Mul"

  (.*) :: ('[EmptyNoteF, MulF] :<<: xs) => EADT xs -> EADT xs -> EADT xs
  (.*) a b = Mul EmptyNote (a,b)

  -- parser
  --------------------------------------------------------

  mulParser :: ('[EmptyNoteF, MulF] :<<: xs) => MParser (EADT xs) -> MParser (EADT xs)
  mulParser factorP = Mul EmptyNote <$> do
    i1 <- factorP
    _ <- string "*"
    i2 <- factorP
    return (i1,i2)
    
  -- Algebra
  --------------------------------------------------------

  instance Algebra MulF where
    showAST' (MulF α (i, v)) = "(" <> i <> " * " <> v <> ")" <> α -- no recursive call

  
  -- Isomorphism
  --------------------------------------------------------
  instance ('[HErrorF, EmptyNoteF, TypF, MulF] :<<: xs
           , AlgVariantF (Isomorphism xs) (EADT xs) xs, Isomorphism xs (VariantF xs)
           , Functor (VariantF xs), Algebra (VariantF xs))
          => Isomorphism xs MulF where
    getAnnotation (MulF α _) = α
    setType' (MulF α (i, v)) =
      case (i, v) of
        (HError _ _, _) -> i
        (_, HError _ _) -> v
        _ -> case (getType i, getType v) of
                (Just TInt, Just TInt)   -> Mul (Typ α TInt  ) (i,v)
                (Just TInt, Just TFloat) -> Mul (Typ α TFloat) (i,v)
                (Just TFloat, Just TInt) -> Mul (Typ α TFloat) (i,v)
                (Just t1  , Just t2)     -> 
                         HError α $ "can't multiply `" <> showAST i <> "` whose type is " <> show t1 <>
                                  " with `" <> showAST v <> "` whose type is " <> show t2
                (_,_) -> HError α "Missing type info in multiplication"

    
  -- apply distribution : a*(b+c) -> (a*b+a*c)
  --------------------------------------------------------

  distribute x = case splitVariantF @'[AddF, MulF] $ unfix x of
    Right leftovers -> leftovers & (fmap d) & liftVariantF & Fix
    Left v          -> variantFToCont v >::>
                          ( \(AddF α (v1,v2)) -> Add α (v1,v2) --TODO
                          , \(MulF α (v1,v2)) -> go α (v1,v2)
                          )
    where
      d v = distribute v
      go α (i, (Add β (v1,v2))) = Add β (d (Mul α (i,d v1)), d (Mul α (i,d v2)))
      go α ((Add β (v1,v2)), i) = Add β (d (Mul α (d v1,i)), d (Mul α (d v2,i)))
      go α (v1,v2)              = Mul α (d v1, d v2)


  -- demultiply : n*a -> a+a+... n times
  --------------------------------------------------------
     
  --TODO : TypF
  demultiply x = case splitVariantF @'[MulF, TypF] $ unfix x of
    Right leftovers -> leftovers & (fmap d) & liftVariantF & Fix
    Left v          -> variantFToCont v >::>
                          ( \(MulF α (v1,v2)) -> go α (v1,v2)
                          , \(TypF α t) -> Typ (d α) t --TODO
                          )
    where 
      d v = demultiply v -- in target AST
      go α (v1,v2) = 
        let 
          go' i v v' = 
                if  | i < 0 -> HError (d α) $ "Error: can't multiply by negative number " 
                              <> show i
                    | i == 0 -> Val (d α) 0
                    | i == 1 -> v'
                    | otherwise -> Add (d α) (d v, d $ Mul α ((Val α $ i-1), v))
        in
          case (d v1, d v2) of
            (HError _ e, _) -> HError (d α) e
            (_, HError _ e) -> HError (d α) e
            (Val _ i1, v2') -> go' i1 v2 v2'
            (v1', Val _ i2) -> go' i2 v1 v1'
            (_, _) -> HError (d α) $ "Can't multiply by " <> showAST v1
