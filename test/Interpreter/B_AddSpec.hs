{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Interpreter.B_AddSpec (spec) where

  import Interpreter.A_Type
  import Interpreter.B_Add
  import Interpreter.Utils

  import Control.Exception (evaluate)
  import Haskus.Utils.EADT
  import Test.Hspec
  import Relude hiding (fromInteger, fromRational)
  
  type AST = EADT '[HErrorF,EmptyNoteF,TypF, ValF,FloatValF,AddF]
  
  spec :: Spec
  spec = do
    describe "B_Add" $ do
      it "shows int value" $ do
        showAST (5 :: AST ) `shouldBe` "5"
      it "shows float value" $ do
        showAST (5.3 :: AST ) `shouldBe` "5.3"
      it "shows add" $ do
        showAST (Add (Typ TInt EmptyNote) (5,3) :: AST ) 
        `shouldBe` "(5 + 3) :: TInt"
      it "sets type" $ do
        showAST ((5 .+ 3) :: AST ) 
        `shouldBe` "(5 + 3)"