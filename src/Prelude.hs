
module Prelude
        ( module Relude
        , MParser
        ) where

import Relude  hiding
        ( many, some
        )

import qualified Debug.Trace as Debug

import Text.Megaparsec
type MParser = Parsec Void Text
