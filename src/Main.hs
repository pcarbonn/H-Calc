-- from http://hsyl20.fr/home/posts/2018-05-22-extensible-adt.html

module Main where

import Utils

import Haskus.Utils.EADT
import Data.Functor.Foldable

-- simplest model : addition only
-------------------------------------------------------
data ValF e = ValF Int deriving (Functor)
data AddF e = AddF e e deriving (Functor)

type AddValADT = EADT '[ValF,AddF]

-- define patterns, for creation and pattern matching

pattern Val :: ValF :<: xs => Int -> EADT xs
pattern Val a = VF (ValF a)

pattern Add :: AddF :<: xs => EADT xs -> EADT xs -> EADT xs
pattern Add a b = VF (AddF a b)

addVal = Add (Val 10) (Val 5) :: AddValADT

-- show

instance ShowEADT ValF where
  showEADT' (ValF i) = show i

instance ShowEADT AddF where
  showEADT' (AddF u v) = "(" <> u <> " + " <> v <> ")" -- no recursive call

showEADT :: (ShowEADT (Base t), Recursive t) => t -> Text -- type inferred by GHC
showEADT = cata showEADT'

-- Eval

instance Eval (ValF e) where
  eval (ValF i) = i

instance EvalAll xs => Eval (AddF (EADT xs)) where
  eval (AddF u v) = evalEADT u + evalEADT v -- explicit recursion
  

-- add Mul
--------------------------------------------------------

data MulF e = MulF e e deriving (Functor)
type MulAddValADT = EADT '[ValF,AddF,MulF]

pattern Mul :: MulF :<: xs => EADT xs -> EADT xs -> EADT xs
pattern Mul a b = VF (MulF a b)

mulAddVal = Mul (Val 2) (Add (Val 10) (Val 3)) :: MulAddValADT

instance ShowEADT MulF where
  showEADT' (MulF u v) = "(" <> u <> " * " <> v <> ")" -- no recursive call

instance EvalAll xs => Eval (MulF (EADT xs)) where
  eval (MulF u v) = evalEADT u * evalEADT v -- explicit recursion  


-- apply distribution : a*(b+c) -> (a*b+a*c)
--------------------------------------------------------

-- distribute multiplication over addition if it matches
distr' :: (AddF :<: f, MulF :<: f) => EADT f -> Maybe (EADT f)
distr' (Mul a (Add b c)) = Just (Add (Mul a b) (Mul a c))
distr' _                 = Nothing

distr = bottomUpFixed distr'

-- simplify multiplication a*b -> a+b
-- (it makes no sense but it's just an example)
--------------------------------------------------------

instance{-# OVERLAPPING #-} AddF :<: ys => Simplify MulF ys where
  simplify (MulF u v) = Add u v

instance {-# OVERLAPPABLE #-} f :<: ys => Simplify f ys where
  simplify = VF  -- the other constructors are kept as-is

-- Add Float
--------------------------------------------------------

data FloatValF e = FloatValF Float deriving (Functor)

pattern FloatVal :: FloatValF :<: xs => Float -> EADT xs
pattern FloatVal a = VF (FloatValF a)

instance ShowEADT FloatValF where
    showEADT' (FloatValF i) = show i

-- Type checker
--------------------------------------------------------
instance TypeCheck ValF ys where
  typeCheck' _ = T "Int"

instance TypeCheck FloatValF ys where
  typeCheck' _ = T "Float"

instance (AddF :<: ys, ShowEADT (VariantF ys), Functor (VariantF ys)) => TypeCheck AddF ys where
  typeCheck' (AddF (u,t1) (v,t2))
    | t1 == t2       = t1
    | TError _ <- t1 = t1 -- propagate errors
    | TError _ <- t2 = t2
    | otherwise = TError $ "can't add `" <> showEADT u <> "` whose type is " <> show t1 <>
                          " with `" <> showEADT v <> "` whose type is " <> show t2


-- Main
--------------------------------------------------------
main :: IO ()
main = do
  putTextLn "hello world"
  putTextLn $ showEADT addVal
  putTextLn $ showEADT mulAddVal
  putTextLn $ show $ evalEADT mulAddVal
  putTextLn $ showEADT (distr mulAddVal)
  putTextLn $ showEADT (cata simplify (mulAddVal) :: AddValADT)
  putTextLn $ showEADT (Add (Val 10) (FloatVal 5) :: EADT '[ValF,FloatValF,AddF])
  putTextLn $ show $ para typeCheck' (Add (Val 10) (FloatVal 5) :: EADT '[ValF,FloatValF,AddF])

