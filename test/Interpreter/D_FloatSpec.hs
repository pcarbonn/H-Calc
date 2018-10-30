{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Interpreter.D_FloatSpec (spec) where

  import Interpreter.A_TypeCheck
  import Interpreter.D_Float
  import Interpreter.Utils
  import Interpreter.Result

  import Control.Exception (evaluate)
  import Haskus.Utils.EADT
  import Test.Hspec
  import Relude hiding (fromInteger, fromRational)
  
  type AST = EADT '[HErrorF,EmptyNoteF, FloatValF]

  spec :: Spec
  spec = do
    describe "D_Float" $ do
      it "shows float value" $ do
        showAST (5.0 :: AST ) `shouldBe` "5.0"
  