{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Interpreter.C_MulSpec (spec) where

  import Interpreter.A_TypeCheck
  import Interpreter.B_Add
  import Interpreter.C_Mul
  import Interpreter.Utils
  import Interpreter.Result

  import Control.Exception (evaluate)
  import Haskus.Utils.EADT
  import Test.Hspec
  import Relude hiding (fromInteger, fromRational)
  
  type AST = EADT '[HErrorF,EmptyNoteF,TypF, ValF,AddF,MulF]

  spec :: Spec
  spec = do
    describe "C_Mul" $ do
      it "shows mul" $ do
        showAST ((5 .* 3) :: AST ) 
        `shouldBe` "(5 * 3)"
  