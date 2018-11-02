{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}

module Interpreter.A_TypeSpec (spec) where

  import Interpreter.A_Type
  import Interpreter.Utils
  import Interpreter.Result

  import Control.Exception (evaluate)
  import Haskus.Utils.EADT
  import Test.Hspec
  import Relude hiding (fromInteger, fromRational)
  
  spec :: Spec
  spec = do
    describe "A_Type" $ do
      it "shows the type" $ do
        showAST (Typ TInt EmptyNote :: EADT '[HErrorF,EmptyNoteF,TypF] ) `shouldBe` " :: TInt"
      it "can't eval type" $ do
        evalAST (Typ TInt EmptyNote :: EADT '[HErrorF,EmptyNoteF,TypF] ) `shouldBe` RError "Can't evaluate annotations"
      it "gets annotation" $ do
        getType (Typ TInt (Typ TInt EmptyNote) :: EADT '[HErrorF,EmptyNoteF,TypF] ) `shouldBe` (Just TInt)