{-# LANGUAGE NoImplicitPrelude #-}
module Interpreter.ResultSpec (spec) where

  import Interpreter.A_Type
  import Interpreter.Utils
  import Interpreter.Result

  import Control.Exception (evaluate)
  import Haskus.Utils.EADT
  import Test.Hspec
  import Relude
  
  spec :: Spec
  spec = do
    describe "Result" $ do
      it "shows result" $ do
        show (RInt 5) `shouldBe` "RInt 5"