-- from http://hsyl20.fr/home/posts/2018-05-22-extensible-adt.html

module Main where

import Interpreter.B_Add
import Interpreter.C_Mul
import Interpreter.D_Float
import Interpreter.Utils
import Interpreter.Result

import Haskus.Utils.EADT
import Data.Functor.Foldable

addVal = Add (Val 10) (Val 5) :: AddValADT
mulAddVal = Mul (Val 3) (Add (Val 10) (Val 3)) :: MulAddValADT
mulAddVal2 = Mul (Val (-3)) (Add (Val 10) (Val 3)) :: MulAddValADT
mulAddValFloat = Mul (Val 10) (FloatVal 5) :: EADT '[HErrorF, ValF,MulF, FloatValF,AddF]

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

  putTextLn $ showAST (demultiply $ Mul (Val (-2)) (Val 5) ::  EADT '[HErrorF,ValF,AddF,MulF])
  
  putText $ showAST mulAddValFloat
  putText " -> "
  putTextLn $ showAST mulAddValFloat


  putTextLn $ show $ para typeCheck' (Add (Val 10) (FloatVal 5) :: EADT '[HErrorF,ValF,FloatValF,AddF])
