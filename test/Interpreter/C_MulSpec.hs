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
  
  type AST  = EADT '[HErrorF,EmptyNoteF,TypF, ValF,AddF,MulF]
  type AST2 = EADT '[HErrorF,EmptyNoteF,TypF, ValF,AddF]

  spec :: Spec
  spec = do
    describe "C_Mul" $ do
      it "shows mul" $ do
        showAST ((5 .* 3) :: AST ) 
        `shouldBe` "(5 * 3)"
      it "distributes" $ do
        showAST (demultiply $ distribute $ ((2 .+ 1) .* 5 :: AST) :: AST2) 
        `shouldBe` "((5 + 5) + 5)"
      it "distributes" $ do
        showAST (demultiply $ distribute $ (2 .* (2 .+ 1) :: AST) :: AST2) 
        `shouldBe` "((2 + 2) + (1 + 1))"

      
      it "distributes" $ do
        showAST (cata demultiply' (2 .* 5 :: AST) :: AST2) `shouldBe` "(5 + 5)"
      it "distributes" $ do
        showAST (cata demultiply' (2 .* (2 .* 5) :: AST) :: AST2) `shouldBe` "((5 + 5) + (5 + 5))"
      it "distributes" $ do
        showAST (cata demultiply' ((2 .* 5) .+ 1 :: AST) :: AST2) `shouldBe` "((5 + 5) + 1)"  