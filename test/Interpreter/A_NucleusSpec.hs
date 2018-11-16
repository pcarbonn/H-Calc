{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}

module Interpreter.A_NucleusSpec (spec) where

  import Interpreter.A_Nucleus
  import Interpreter.Transfos
  import Interpreter.Interpreter

  import Control.Exception (evaluate)
  import Test.Hspec
  import Relude hiding (fromInteger, fromRational)

  spec :: Spec
  spec = do
    describe "A_Type" $ do
      it "says ok" $ do
        "ok" `shouldBe` "ok"
