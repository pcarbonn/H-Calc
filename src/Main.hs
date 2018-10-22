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

e = emptyAnnot
addVal = Add e (Val e 10, Val e 5) :: AddValADT
mulAddVal = Mul e (Val e 3, Add e (Val e 10, Val e 3)) :: MulAddValADT
mulAddVal2 = Mul e (Val e (-3), Add e (Val e 10, Val e 3)) :: MulAddValADT
mulAddValFloat = Mul e (Val e 10, FloatVal e 5) :: EADT '[HErrorF, ValF,MulF, FloatValF,AddF]

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

  putTextLn $ showAST (demultiply $ Mul e (Val e (-2), Val e 5) ::  EADT '[HErrorF,ValF,AddF,MulF])
  
  putText $ showAST mulAddValFloat
  putText " -> "
  putTextLn $ showAST mulAddValFloat


  putTextLn $ show $ para typeCheck' (Add e (Val e 10, FloatVal e 5) :: EADT '[HErrorF,ValF,FloatValF,AddF])
