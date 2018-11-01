-- you need to adapt H-Calc.cabal to run this program: 
--  hs-source-dirs:     sandbox
--  main-is:            Sandbox.hs


module Sandbox where

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
  
  removeOddEvenS :: EADT '[EvenF Int, OddF Int, NilF]
                    -> EADT '[ConsF Int, NilF]
  removeOddEvenS = cataRemove

  eo :: EADT '[EvenF Int, OddF Int, NilF]
  eo = Even (10 :: Int) $ Odd (5 :: Int) $ Odd (7 :: Int) Nil

  main :: IO ()
  main = do
    putStrLn $ eadtShow (cata removeOddEven eo :: List Int)