-- from http://hsyl20.fr/home/posts/2018-05-22-extensible-adt.html

module Main where

import A_Error
import B_Add
import C_Mul
import Utils

import Haskus.Utils.EADT
import Data.Functor.Foldable

addVal = Add (Val 10) (Val 5) :: AddValADT
mulAddVal = Mul (Val 3) (Add (Val 10) (Val 3)) :: MulAddValADT
mulAddVal2 = Mul (Val (-3)) (Add (Val 10) (Val 3)) :: MulAddValADT

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

  putText $ showEADT mulAddVal
  putText " = "
  putTextLn $ showEADT $ demultiply (mulAddVal2)
