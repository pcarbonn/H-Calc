{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Interpreter.TransfosSpec (spec) where

  import Interpreter.A_Nucleus
  import Interpreter.Transfos

  import Control.Exception (evaluate)
  import Test.Hspec
  import Relude hiding (fromInteger, fromRational)

  spec :: Spec
  spec = do
    describe "Utils" $ do
      it "says ok" $ do
        "ok" `shouldBe` "ok"
