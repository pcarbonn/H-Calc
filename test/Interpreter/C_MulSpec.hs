{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Interpreter.C_MulSpec (spec) where

  import Interpreter.A_Nucleus
  import Interpreter.B_Add
  import Interpreter.C_Mul
  import Interpreter.Transfos

  import Control.Exception (evaluate)
  import Haskus.Utils.EADT
  import Test.Hspec
  import Relude hiding (fromInteger, fromRational)
  
  type AST  = EADT '[HErrorF,EmptyNoteF, ValF,FloatValF,AddF,MulF,TypF]
  type AST2 = EADT '[HErrorF,EmptyNoteF, ValF,FloatValF,AddF,     TypF]

  spec :: Spec
  spec = do
    describe "C_Mul" $ do
      it "shows mul" $ do
        showAST ((5 .* 3) :: AST ) 
        `shouldBe` "(5 * 3)"
      it "distributes 1" $ do
        showAST (demultiply $ distribute $ ((2 .+ 1) .* 5 :: AST) :: AST2) 
        `shouldBe` "((5 + 5) + 5)"
      it "distributes 2" $ do
        showAST (demultiply $ distribute $ (2 .* (2 .+ 1) :: AST) :: AST2) 
        `shouldBe` "((2 + 2) + (1 + 1))"

      
      it "distributes 3" $ do
        showAST (demultiply(2 .* 5 :: AST) :: AST2) `shouldBe` "(5 + 5)"
      it "distributes 4" $ do
        showAST (demultiply(2 .* (2 .* 5) :: AST) :: AST2) `shouldBe` "((5 + 5) + (5 + 5))"
      it "distributes 5" $ do
        showAST (demultiply((2 .* 5) .+ 1 :: AST) :: AST2) `shouldBe` "((5 + 5) + 1)"  