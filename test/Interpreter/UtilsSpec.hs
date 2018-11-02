{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Interpreter.UtilsSpec (spec) where

  import Interpreter.A_Type
  import Interpreter.Utils
  import Interpreter.Result

  import Control.Exception (evaluate)
  import Haskus.Utils.EADT
  import Test.Hspec
  import Relude hiding (fromInteger, fromRational)

  spec :: Spec
  spec = do
    describe "Utils" $ do
      it "shows EmptyNote" $ do
        showAST (EmptyNote :: EADT '[HErrorF,EmptyNoteF] ) `shouldBe` ""
