module ParserUtils 
  ( intLine
  , intGroup
  , prtParserError
  , strLine
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

prtParserError :: String -> IO()
prtParserError s = print ("Parser error: " <> s)

strLine :: A.Parser String
strLine = do
  s <- A.many1 (A.notChar '\n')
  _ <- A.endOfLine <|> A.endOfInput
  pure s