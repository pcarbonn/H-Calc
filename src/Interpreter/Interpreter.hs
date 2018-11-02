module Interpreter.Interpreter where

  -- this module is the main entry point of the interpreter

  import Interpreter.A_Type
  import Interpreter.B_Add
  import Interpreter.C_Mul
  import Interpreter.Utils
  import Interpreter.Result

  import Haskus.Utils.ContFlow
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

  -- evaluation
  --------------------------------------------------------

  eval :: EADT '[ValF, FloatValF, AddF] -> Result
  eval l = eadtToCont l >::>
      ( \(ValF _ i) -> RInt i
      , \(FloatValF _ f) -> RFloat f
      , \(AddF _ (v1,v2)) -> go (eval v1) (eval v2))
      where 
        go v1 v2 =
          case (v1, v2) of -- implicit recursion
            (RInt v1', RInt v2') -> RInt (v1'+v2')
            (RFloat v1', RFloat v2') -> RFloat (v1'+v2')
            (RError e, _) -> RError e
            (_, RError e) -> RError e
            (a,b)             -> RError $ "Error in eval(" <> show a <> "+" <> show b <> ")"

  -- interpret
  --------------------------------------------------------
  
  interpret :: Text -> Result
  interpret source
    = case runParser parser "" source of
        Left _ -> RError "can't parse"
        Right a -> evalAST $ demultiplyS $ distribute $ setType $ appendEADT @'[HErrorF, TypF] a 