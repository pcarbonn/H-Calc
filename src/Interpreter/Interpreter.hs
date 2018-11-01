module Interpreter.Interpreter where

  -- this module is the main entry point of the interpreter

  import Interpreter.A_TypeCheck
  import Interpreter.B_Add
  import Interpreter.C_Mul
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

  -- type specialisation

  type AST1 = EADT '[EmptyNoteF, ValF, AddF, MulF, FloatValF,HErrorF, TypF]
  type AST2 = EADT '[EmptyNoteF, ValF, AddF,       FloatValF,HErrorF, TypF]

  demultiplyS :: AST1
              -> AST2
  demultiplyS = demultiply

  -- interpret
  --------------------------------------------------------
  
  interpret :: Text -> Result
  interpret source
    = case runParser parser "" source of
        Left e -> RError "can't parse"
        Right a -> evalAST $ demultiplyS $ distribute $ setType $ appendEADT @'[HErrorF, TypF] a 