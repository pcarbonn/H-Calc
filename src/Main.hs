-- from http://hsyl20.fr/home/posts/2018-05-22-extensible-adt.html

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

ε = emptyAnnot

fromInteger :: ValF :<: f => Integer -> EADT f
fromInteger i = Val ε $ fromIntegral i
fromRational :: FloatValF :<: f => Rational -> EADT f
fromRational i = FloatVal ε $ realToFrac i

(.+) :: AddF :<: f => EADT f -> EADT f -> EADT f
(.+) a b = Add ε (a,b)
(.*) a b = Mul ε (a,b)

neg :: ValF :<: f => EADT f -> EADT f
neg (Val α i)= Val α (-i)



-- Main

main :: IO ()
main = do
  let addVal = 10 .+ 5 :: AddValADT
  let mulAddVal = 3 .* (10 .+ 3) :: MulAddValADT
  let mulAddVal2 = (neg 3) .* (10 .+ 3) :: MulAddValADT
  let mulAddValFloat = 10.* 5.0 :: EADT '[HErrorF, ValF,MulF, FloatValF,AddF]
  
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

  putTextLn $ showAST (demultiply $ (neg 2 .* 5) ::  EADT '[HErrorF,ValF,AddF,MulF])
  
  putText $ showAST mulAddValFloat
  putText " -> "
  putTextLn $ showAST mulAddValFloat


  putTextLn $ show $ para typeCheck' (10 .+ 5.0 :: EADT '[HErrorF,ValF,FloatValF,AddF])
