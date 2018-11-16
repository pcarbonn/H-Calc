
module Interpreter.A_Nucleus where

  -- this module defines the following nodes
  --    (HError Text)
  --    (EmptyNote)
  --    (Typ t α)
  -------------------------------------------------------

  import Interpreter.Transfos

  import Fmt
  import Text.Megaparsec
  import Text.Megaparsec.Char as M
  import qualified Text.Megaparsec.Char.Lexer as L
  import Text.Show


  -- parser
  --------------------------------------------------------
  spaceConsumer :: MParser ()
  spaceConsumer = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

  symbol :: Text -> MParser Text
  symbol = L.symbol spaceConsumer



  -- AST nodes
  -------------------------------------------------------



  -- Transformations
  --------------------------------------------------------

  -- algebra



  -- other
