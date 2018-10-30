-- from http://hsyl20.fr/home/posts/2018-05-22-extensible-adt.html

{-# LANGUAGE RebindableSyntax #-}

module Main where

import Interpreter.A_TypeCheck
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


-- Main

type V1_AST = EADT '[HErrorF,EmptyNoteF, ValF,AddF]
type V2_AST = EADT '[HErrorF,EmptyNoteF, ValF,AddF,MulF]
type V3_AST = EADT '[HErrorF,EmptyNoteF, ValF,AddF,MulF,FloatValF]

main :: IO ()
main = do
  let addVal = 10 .+ 5 :: V1_AST
  let mulAddVal = 3 .* (10 .+ 3) :: V2_AST
  let mulAddVal2 = (neg 3) .* (10 .+ 3) :: V2_AST
  let mulAddValFloat = 10.* 5.0 :: V3_AST
  
  putText $ showAST addVal
  putText " = "
  putTextLn $ show $ evalAST addVal

  putText $ showAST mulAddVal
  putText " = "
  putTextLn $ show $ evalAST mulAddVal -- without demultiply !

  putText $ showAST mulAddVal
  putText " -> "
  putTextLn $ show $ evalAST (demultiply $ distribute $ mulAddVal) -- OK

  putTextLn $ showAST $ demultiply $ distribute $ ((neg 2 .* 5) ::  V2_AST)

  putText $ showAST mulAddVal2
  putText " = "
  putTextLn $ showAST $ demultiply $ distribute $ mulAddVal2
  
  putText $ showAST mulAddValFloat
  putText " -> "
  putTextLn $ showAST mulAddValFloat


  putTextLn $ showAST $ setType $ appendEADT @'[TypF] ((3 .+ 5).*5.0 :: V3_AST)

  putTextLn $ show $ showAST <$> parseMaybe parser "(3+1)*15.0"
  putTextLn $ show $ interpret "((2+1)*5.0)"
