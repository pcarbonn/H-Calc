-- from http://hsyl20.fr/home/posts/2018-05-22-extensible-adt.html

module Main where

import A_Error
import B_Add
import C_Mul
import D_Float
import Utils

import Haskus.Utils.EADT
import Data.Functor.Foldable

addVal = Add (Val 10) (Val 5) :: AddValADT
mulAddVal = Mul (Val 3) (Add (Val 10) (Val 3)) :: MulAddValADT
mulAddVal2 = Mul (Val (-3)) (Add (Val 10) (Val 3)) :: MulAddValADT
mulAddValFloat = Mul (Val 10) (FloatVal 5) :: EADT '[HErrorF, ValF,MulF, FloatValF,AddF]

main :: IO ()
main = do
  putText $ showEADT addVal
  putText " = "
  putTextLn $ show $ evalEADT addVal

  putText $ showEADT mulAddVal
  putText " = "
  putTextLn $ show $ evalEADT mulAddVal

  putText $ showEADT mulAddVal
  putText " -> "
  putTextLn $ showEADT (demultiply mulAddVal)

  putText $ showEADT mulAddVal2
  putText " = "
  putTextLn $ showEADT $ demultiply (mulAddVal2)

  putTextLn $ showEADT (demultiply $ Mul (Val (-2)) (Val 5) ::  EADT '[HErrorF,ValF,AddF,MulF])
  putText $ showEADT mulAddValFloat
  putText " -> "
  putTextLn $ showEADT mulAddValFloat
  putTextLn $ show $ para typeCheck' (Add (Val 10) (FloatVal 5) :: EADT '[HErrorF,ValF,FloatValF,AddF])
