{-# LANGUAGE NoImplicitPrelude #-}
module Interpreter.C_MulSpec (spec) where

  import Test.Hspec
  import Control.Exception (evaluate)
  import Relude
  
  spec :: Spec
  spec = do
    describe "Prelude.head" $ do
      it "returns the first element of a list" $ do
        viaNonEmpty head [23 ..] `shouldBe` (Just $ 23 :: Maybe Int)
  