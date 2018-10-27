-- from http://hsyl20.fr/home/posts/2018-05-22-extensible-adt.html

{-# LANGUAGE RebindableSyntax #-}

module Main where

import Interpreter.A_Annotation
import Interpreter.B_Add
import Interpreter.C_Mul
import Interpreter.D_Float
import Interpreter.Interpreter
import Interpreter.Utils
import Interpreter.Result

import Haskus.Utils.EADT
import Prelude hiding (fromInteger, fromRational)
import Text.Megaparsec
import Text.Megaparsec.Char as M

-- syntactic sugar


fromInteger :: (EmptyNoteF :<: xs, ValF :<: xs) => Integer -> EADT xs
fromInteger i = Val EmptyNote $ fromIntegral i
fromRational :: (EmptyNoteF :<: xs, FloatValF :<: xs) => Rational -> EADT xs
fromRational i = FloatVal EmptyNote $ realToFrac i

(.+) :: (EmptyNoteF :<: xs, AddF :<: xs) => EADT xs -> EADT xs -> EADT xs
(.+) a b = Add EmptyNote (a,b)
(.*) a b = Mul EmptyNote (a,b)

neg :: ValF :<: xs => EADT xs -> EADT xs
neg (Val α i)= Val α (-i)


type AddValADT = EADT '[EmptyNoteF,ValF,AddF]
type MulAddValADT = EADT '[HErrorF,EmptyNoteF, ValF,AddF,MulF]

-- Main

main :: IO ()
main = do
  let addVal = 10 .+ 5 :: AddValADT
  let mulAddVal = 3 .* (10 .+ 3) :: MulAddValADT
  let mulAddVal2 = (neg 3) .* (10 .+ 3) :: MulAddValADT
  let mulAddValFloat = 10.* 5.0 :: EADT '[HErrorF,EmptyNoteF, ValF,FloatValF,AddF,MulF]
  
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


  putTextLn $ showAST $ setType $ appendEADT @'[TTypeF] ((3 .+ 5).*5.0 :: EADT '[HErrorF,EmptyNoteF,ValF,FloatValF,AddF,MulF])

  putTextLn $ show $ showAST <$> parseMaybe parser "(3+1)*15.0"
  putTextLn $ show $ interpret "((2+1)*5.0)"
