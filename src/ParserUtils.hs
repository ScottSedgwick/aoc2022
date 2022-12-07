module ParserUtils 
  ( eol
  , intLine
  , intGroup
  , prtParserError
  , restOfLine
  , string
  , strLine
  ) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

eol :: A.Parser ()
eol = A.endOfLine <|> A.endOfInput

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

restOfLine :: A.Parser String
restOfLine = A.many1 (A.satisfy (/= '\n'))

string :: String -> A.Parser T.Text
string s = A.string (T.pack s) 

strLine :: A.Parser String
strLine = do
  s <- A.many1 (A.notChar '\n')
  _ <- A.endOfLine <|> A.endOfInput
  pure s