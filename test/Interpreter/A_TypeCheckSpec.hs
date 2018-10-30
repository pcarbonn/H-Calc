{-# LANGUAGE NoImplicitPrelude #-}

module Interpreter.A_TypeCheckSpec (spec) where

  import Interpreter.A_TypeCheck
  import Interpreter.Utils
  import Interpreter.Result

  import Control.Exception (evaluate)
  import Haskus.Utils.EADT
  import Test.Hspec
  import Relude
  
  spec :: Spec
  spec = do
    describe "A_TypeCheck" $ do
      it "shows the type" $ do
        showAST (Typ TInt EmptyNote :: EADT '[HErrorF,EmptyNoteF,TypF] ) `shouldBe` " :: TInt"
      it "can't eval type" $ do
        evalAST (Typ TInt EmptyNote :: EADT '[HErrorF,EmptyNoteF,TypF] ) `shouldBe` RError "Can't evaluate annotations"