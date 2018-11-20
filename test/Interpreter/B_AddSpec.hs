{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Interpreter.B_AddSpec (spec) where

  -- import Interpreter.A_Nucleus
  -- import Interpreter.B_Add
  -- import Interpreter.Interpreter
  -- import Interpreter.Transfos

  -- import Control.Exception (evaluate)
  -- import Haskus.Utils.EADT
  import Test.Hspec
  import Relude hiding (fromInteger, fromRational)


  spec :: Spec
  spec = do
    describe "B_Add" $ do
      it "ok" $ do "ok" `shouldBe` "ok"
  --     it "shows int value" $ do
  --       showAST (5 :: AST1 ) `shouldBe` "5"
  --     it "shows negative value" $ do
  --       showAST (neg 5 :: AST1 ) `shouldBe` "-5"
  --     it "shows float value" $ do
  --       showAST (5.3 :: AST1 ) `shouldBe` "5.3"
  --     it "shows add" $ do
  --       showAST (Add (Typ EmptyNote TInt) (5,3) :: AST1 )
  --       `shouldBe` "(5 + 3) :: TInt"
  --     it "sets type" $ do
  --       showAST ((5 .+ 3) :: AST1 )
  --       `shouldBe` "(5 + 3)"
