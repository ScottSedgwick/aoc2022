module ParserUtils 
  ( intLine
  , intGroup
  ) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as A

intLine :: Integral a => A.Parser a
intLine = do
  x <- A.decimal
  A.endOfLine <|> A.endOfInput
  pure x

intGroup :: Integral a => A.Parser [a]
intGroup = do
  x <- A.many1 intLine
  A.endOfLine
  pure x
