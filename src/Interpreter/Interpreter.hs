module Interpreter.Interpreter where

  -- this module is the main entry point of the interpreter

  import Interpreter.A_Nucleus
  import Interpreter.B_Add
  import Interpreter.C_Mul
  import Interpreter.Transfos

  import Haskus.Utils.ContFlow
  import Haskus.Utils.EADT
  import Text.Megaparsec
  import Text.Megaparsec.Char as M
  
  -- main parser
  --------------------------------------------------------

  termParser :: MParser (EADT '[HErrorF, EmptyNoteF, ValF, FloatValF, AddF, MulF])
  termParser
    = try floatValParser
    <|> valParser
    <|> do
          _ <- string "("
          e <- parser
          _ <- string ")"
          return e


  factorParser :: MParser (EADT '[HErrorF, EmptyNoteF, ValF, FloatValF, AddF, MulF])  
  factorParser
    = try (mulParser termParser)
    <|> termParser

  parser :: MParser (EADT '[HErrorF, EmptyNoteF, ValF, FloatValF, AddF, MulF])
  parser 
    = try (addParser factorParser)
    <|> factorParser


  -- evaluation
  --------------------------------------------------------

  data Result 
    = RInt Int
    | RFloat Float
    | RError Text
    deriving (Show, Eq)

  eval :: EADT '[HErrorF, EmptyNoteF, ValF, FloatValF, AddF] -> Result
  eval l = eadtToCont l >::>
      ( \(HErrorF t)      -> RError t
      , \(EmptyNoteF)     -> RError "can't evaluate empty expression"
      , \(ValF _ i)       -> RInt i
      , \(FloatValF _ f)  -> RFloat f
      , \(AddF _ (v1,v2)) -> go (eval v1) (eval v2))
      where 
        go v1 v2 =
          case (v1, v2) of -- implicit recursion
            (RInt v1', RInt v2')     -> RInt (v1'+v2')
            (RFloat v1', RFloat v2') -> RFloat (v1'+v2')
            (RError e, _) -> RError e
            (_, RError e) -> RError e
            (a,b)         -> RError $ "Error in eval(" <> show a <> "+" <> show b <> ")"


  -- type specialisation
  --------------------------------------------------------

  type AST2 = EADT '[HErrorF, EmptyNoteF, ValF, FloatValF, AddF, MulF, TypF]
  type AST1 = EADT '[HErrorF, EmptyNoteF, ValF, FloatValF, AddF,       TypF]
  type AST0 = EADT '[HErrorF, EmptyNoteF, ValF, FloatValF, AddF            ]

  demultiplyS :: AST2 -> AST1
  demultiplyS = demultiply

  removeAnnotationS :: AST1 -> AST0
  removeAnnotationS = removeAnnotation


  -- interpret
  --------------------------------------------------------
  
  interpret :: Text -> Result
  interpret source
    = case runParser parser "" source of
        Left _ -> RError "can't parse"
        Right a -> a
            & appendEADT @'[TypF] & setType 
            & distribute & demultiplyS 
            & removeAnnotationS & eval