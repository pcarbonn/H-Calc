module Interpreter.Interpreter where

  import Interpreter.A_TypeCheck
  import Interpreter.B_Add
  import Interpreter.C_Mul
  import Interpreter.D_Float
  import Interpreter.Utils
  import Interpreter.Result

  import Haskus.Utils.EADT
  import Text.Megaparsec
  import Text.Megaparsec.Char as M
  
  -- main parser
  --------------------------------------------------------

  termParser :: MParser (EADT '[EmptyNoteF, ValF, AddF, MulF, FloatValF])
  termParser
    = try floatValParser
    <|> valParser
    <|> do
          _ <- string "("
          e <- parser
          _ <- string ")"
          return e


  factorParser :: MParser (EADT '[EmptyNoteF, ValF, AddF, MulF, FloatValF])  
  factorParser
    = try (mulParser termParser)
    <|> termParser

  parser :: MParser (EADT '[EmptyNoteF, ValF, AddF, MulF, FloatValF])
  parser 
    = try (addParser factorParser)
    <|> factorParser

    

  -- interpret
  --------------------------------------------------------
  
  interpret :: Text -> Result
  interpret source
    = case runParser parser "" source of
        Left e -> RError "can't parse"
        Right a -> evalAST $ demultiply $ setType $ appendEADT @'[HErrorF, TTypeF] a 