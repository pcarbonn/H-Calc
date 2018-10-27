
module Sandbox where

  import Haskus.Utils.EADT
  import Prelude

  import Text.Megaparsec
  import Text.Megaparsec.Char as M
  type MParser = Parsec Void Text

  data NatF i 
    = I Int -- term
    | Succ i -- function application
    deriving Show

  type Nat' = Fix NatF

  showNat (Fix (I i)) = show i
  showNat (Fix (Succ i)) = "succ(" <> showNat i <> ")"

  oneN :: Nat'
  oneN = Fix $ I 1
  twoN = Fix $ Succ(oneN)

  intParser :: MParser Nat' -> MParser Nat'
  intParser _ = Fix . I . toInt <$> some M.digitChar
    where toInt :: [Char] -> Int
          toInt cs = foldl' (\a i -> a * 10 + digitToInt i) 0  cs

          digitToInt :: Char -> Int
          digitToInt c = ord c - ord '0'
  
  sucParser natP = Fix . Succ <$> do
    _ <- string "succ("
    i <- natP
    _ <- string ")"
    return i

  natParser 
    = intParser natParser
    <|> sucParser natParser

  main :: IO ()
  main = do
    putTextLn $ showNat twoN
    putTextLn $ show $ showNat <$> parseMaybe natParser "succ(23)"
    putTextLn "ok"