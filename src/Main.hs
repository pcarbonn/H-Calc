-- from http://hsyl20.fr/home/posts/2018-05-22-extensible-adt.html

{-# LANGUAGE RebindableSyntax #-}

module Main where

import Interpreter.A_Annotation
import Interpreter.B_Add
import Interpreter.C_Mul
import Interpreter.D_Float
import Interpreter.Utils
import Interpreter.Result

import Haskus.Utils.EADT
import Prelude hiding (fromInteger, fromRational)

-- syntactic sugar


fromInteger :: (EmptyNoteF :<: f, ValF :<: f) => Integer -> EADT f
fromInteger i = Val EmptyNote $ fromIntegral i
fromRational :: (EmptyNoteF :<: f, FloatValF :<: f) => Rational -> EADT f
fromRational i = FloatVal EmptyNote $ realToFrac i

(.+) :: (EmptyNoteF :<: f, AddF :<: f) => EADT f -> EADT f -> EADT f
(.+) a b = Add EmptyNote (a,b)
(.*) a b = Mul EmptyNote (a,b)

neg :: ValF :<: f => EADT f -> EADT f
neg (Val α i)= Val α (-i)



-- Main

main :: IO ()
main = do
  let addVal = 10 .+ 5 :: AddValADT
  let mulAddVal = 3 .* (10 .+ 3) :: MulAddValADT
  let mulAddVal2 = (neg 3) .* (10 .+ 3) :: MulAddValADT
  let mulAddValFloat = 10.* 5.0 :: EADT '[HErrorF, EmptyNoteF, ValF,MulF, FloatValF,AddF]
  
  putText $ showAST addVal
  putText " = "
  putTextLn $ show $ evalAST addVal

  putText $ showAST mulAddVal
  putText " = "
  putTextLn $ show $ evalAST mulAddVal

  putText $ showAST mulAddVal
  putText " -> "
  putTextLn $ showAST (demultiply mulAddVal)

  putText $ showAST mulAddVal2
  putText " = "
  putTextLn $ showAST $ demultiply (mulAddVal2)

  putTextLn $ showAST (demultiply $ (neg 2 .* 5) ::  EADT '[HErrorF,EmptyNoteF,ValF,AddF,MulF])
  
  putText $ showAST mulAddValFloat
  putText " -> "
  putTextLn $ showAST mulAddValFloat


  putTextLn $ showAST $ setType (10 .+ 5.0 :: EADT '[HErrorF,EmptyNoteF,TypF,ValF,FloatValF,AddF])
