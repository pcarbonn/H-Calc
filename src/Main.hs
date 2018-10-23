-- from http://hsyl20.fr/home/posts/2018-05-22-extensible-adt.html

module Main where

import Interpreter.A_Annotation
import Interpreter.B_Add
import Interpreter.C_Mul
import Interpreter.D_Float
import Interpreter.Utils
import Interpreter.Result

import Haskus.Utils.EADT
import Data.Functor.Foldable

ε = emptyAnnot
addVal = Add ε (Val ε 10, Val ε 5) :: AddValADT
mulAddVal = Mul ε (Val ε 3, Add ε (Val ε 10, Val ε 3)) :: MulAddValADT
mulAddVal2 = Mul ε (Val ε (-3), Add ε (Val ε 10, Val ε 3)) :: MulAddValADT
mulAddValFloat = Mul ε (Val ε 10, FloatVal ε 5) :: EADT '[HErrorF, ValF,MulF, FloatValF,AddF]

main :: IO ()
main = do
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

  putTextLn $ showAST (demultiply $ Mul ε (Val ε (-2), Val ε 5) ::  EADT '[HErrorF,ValF,AddF,MulF])
  
  putText $ showAST mulAddValFloat
  putText " -> "
  putTextLn $ showAST mulAddValFloat


  putTextLn $ show $ para typeCheck' (Add ε (Val ε 10, FloatVal ε 5) :: EADT '[HErrorF,ValF,FloatValF,AddF])
