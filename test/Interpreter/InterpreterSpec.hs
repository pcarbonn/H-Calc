{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Interpreter.InterpreterSpec (spec) where

  import Interpreter.A_Nucleus
  import Interpreter.B_Add
  import Interpreter.C_Mul
  import Interpreter.Interpreter
  import Interpreter.Transfos

  import Control.Exception (evaluate)
  import Test.Hspec
  import Relude hiding (fromInteger, fromRational)

  spec :: Spec
  spec = do
    describe "Interpreter" $ do
      it "says ok" $ do
        "ok" `shouldBe` "ok"
