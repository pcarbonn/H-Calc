-- you need to adapt H-Calc.cabal to run this program: 
--  main-is:            Sandbox.hs


module Sandbox where
  
  import Haskus.Utils.ContFlow
  import Haskus.Utils.EADT hiding (Cons, Nil)
  import Haskus.Utils.EADT.TH
  import Prelude
  
  -- constructor
  -----------------------
  
  data ConsF a e = ConsF a e deriving (Functor)
  data NilF    e = NilF      deriving (Functor)
  data EvenF a l = EvenF a l deriving (Functor)
  data OddF a l  = OddF a l deriving (Functor)

  
  eadtPattern 'ConsF "Cons"
  eadtPattern 'NilF  "Nil"
  
  eadtPattern 'EvenF  "Even"
  eadtPattern 'OddF  "Odd"

  type List a = EADT '[ConsF a, NilF]

  showCont' l = eadtToCont l >::>
    ( \(ConsF a r) -> show a <> " : " <> showCont' r -- explicit recursion
    , \NilF        -> "Nil"
    )

  -- showEADT
  ----------------------------
  class FunctorShow (f :: * -> *) where
    functorShow :: f String -> String

  instance FunctorShow NilF where
    functorShow _ = "Nil"
 
  instance (Show a) => FunctorShow (ConsF a) where
    functorShow (ConsF a l) = show a ++ " : " ++ l

  instance (AlgVariantF FunctorShow String xs) => FunctorShow (VariantF xs) where
    functorShow = algVariantF @FunctorShow functorShow

  eadtShow ::
      ( Functor (VariantF xs)
      , FunctorShow (VariantF xs)
      ) => EADT xs -> String
  eadtShow = cata functorShow

  -- removal
  -----------------------------

  class RemoveOddEven ys (f :: * -> *) where
    removeOddEven :: f (EADT ys) -> EADT ys
 
  -- replace Odd and Even with Cons
  instance ConsF a :<: ys => RemoveOddEven ys (OddF a) where
    removeOddEven (OddF a l) = Cons a l
 
  instance ConsF a :<: ys => RemoveOddEven ys (EvenF a) where
    removeOddEven (EvenF a l) = Cons a l
 
  -- handle the combinator
  instance
      ( AlgVariantF (RemoveOddEven ys) (EADT ys) xs
      ) => RemoveOddEven ys (VariantF xs)
      where
        removeOddEven = algVariantF @(RemoveOddEven ys) removeOddEven
 
  -- handle remaining constructors
  instance {-# OVERLAPPABLE #-} f :<: ys => RemoveOddEven ys f where
      removeOddEven = VF -- keep the other constructors as is
    
  cataRemove :: (Functor (VariantF xs), RemoveOddEven ys (VariantF xs)) => EADT xs -> EADT ys
  cataRemove = cata removeOddEven

  -- using splitVariantF
  ----------------------------------------
  alg x = case splitVariantF @'[EvenF Int, OddF Int] x of
    Left v          -> variantFToCont v >::>
                         ( \(EvenF _ l) -> "Even : " <> l
                         , \(OddF _ l)  -> "Odd : " <> l
                         )
    Right _ -> "something else"

  cataAlg :: EADT '[EvenF Int, OddF Int, ConsF Int, NilF]
    -> Text
  cataAlg = cata alg
   
  -- with cata
  alg2 x = case splitVariantF @'[EvenF Int, OddF Int] x of
    Left v          -> variantFToCont v >::>
                         ( \(EvenF a l) -> Cons a l
                         , \(OddF a l)  -> Cons a l
                         )
    Right leftovers -> Fix (liftVariantF leftovers)

  cataAlg2 :: EADT '[EvenF Int, OddF Int, ConsF Int, NilF]
          -> EADT '[                      ConsF Int, NilF]
  cataAlg2 = cata alg2

  -- without cata

  alg3 :: EADT '[EvenF Int, OddF Int, ConsF Int, NilF]
          -> EADT '[                      ConsF Int, NilF]
  alg3 x = case splitVariantF @'[EvenF Int, OddF Int] $ unfix x of
    Left v          -> variantFToCont v >::>
                         ( \(EvenF a l) -> Cons a (alg3 l)
                         , \(OddF a l)  -> Cons a (alg3 l)
                         )
    Right leftovers -> Fix (liftVariantF $ fmap alg3 leftovers) --TODO

  -- using popVariantF
  ----------------------------------------
  -- without cata

  alg4 :: EADT '[EvenF Int, OddF Int, ConsF Int, NilF]
          -> EADT '[        OddF Int, ConsF Int, NilF]
  alg4 x = case popVariantF @(EvenF Int) $ unfix x of
    Right (EvenF a l) -> Cons a (alg4 l)
    Left over -> Fix (liftVariantF $ fmap alg4 over)

  -- type specialisation
  ---------------------------------
  
  removeOddEvenS :: EADT '[EvenF Int, OddF Int, NilF]
                    -> EADT '[ConsF Int, NilF]
  removeOddEvenS = cataRemove


  -- constants
  ---------------------------------

  eo :: EADT '[EvenF Int, OddF Int, NilF]
  eo = Even (10 :: Int) $ Odd (5 :: Int) $ Odd (7 :: Int) Nil

  eo2 :: EADT '[ConsF Int, EvenF Int, OddF Int, NilF]
  eo2 = Even (10 :: Int) $ Odd (5 :: Int) $ Cons (7 :: Int) $ Odd (7 :: Int) Nil

  intList :: List Int
  intList = Cons (10 :: Int) $ Cons (20 :: Int) $ Cons (30 :: Int) Nil 

  main :: IO ()
  main = do
    putStrLn $ eadtShow (cata removeOddEven eo :: List Int)
    putStrLn $ showCont' intList
    putStrLn $ cata alg eo2
    