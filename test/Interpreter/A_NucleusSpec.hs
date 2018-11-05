{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}

module Interpreter.A_NucleusSpec (spec) where

  import Interpreter.A_Nucleus
  import Interpreter.Transfos
  import Interpreter.Interpreter

  import Control.Exception (evaluate)
  import Haskus.Utils.EADT
  import Test.Hspec
  import Relude hiding (fromInteger, fromRational)
  
  spec :: Spec
  spec = do
    describe "A_Type" $ do
      it "shows the type" $ do
        showAST (Typ EmptyNote TInt :: EADT '[HErrorF,EmptyNoteF,TypF] ) `shouldBe` " :: TInt"
      it "gets annotation" $ do
        getType (Typ (Typ EmptyNote TInt) TInt :: EADT '[HErrorF,EmptyNoteF,TypF] ) `shouldBe` (Just TInt)