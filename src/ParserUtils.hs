module ParserUtils 
  ( digitLine
  , eol
  , ignore
  , int
  , intLine
  , intGroup
  , pEither
  , prtParserError
  , restOfLine
  , string
  , strLine
  , strN
  ) where

import Control.Applicative ((<|>))
import qualified Text.Trifecta as A
import Data.Char (digitToInt)
import qualified Data.Text as T

digitLine :: A.Parser [Int]
digitLine = do
    xs <- A.many A.digit
    _ <- A.restOfLine
    pure $ map digitToInt xs

eol :: A.Parser ()
eol = A.restOfLine >> pure ()

ignore :: A.Parser a -> A.Parser ()
ignore p = do
    _ <- p
    pure ()

int :: Integral a => A.Parser a
int = do
  sign <- A.option '+' (A.char '-')
  x <- fromIntegral <$> A.decimal
  pure $ if (sign == '-') then x * (-1) else x

intLine :: Integral a => A.Parser a
intLine = do
  x <- int
  A.restOfLine
  pure x

intGroup :: Integral a => A.Parser [a]
intGroup = do
  x <- A.many intLine
  A.restOfLine
  pure x

prtParserError :: A.ErrInfo -> IO()
prtParserError e = print ("Parser error: " <> show e)

restOfLine :: A.Parser String
restOfLine = A.many (A.satisfy (/= '\n'))

string :: String -> A.Parser T.Text
string s = T.pack <$> A.string s

strLine :: A.Parser String
strLine = do
  s <- A.some (A.notChar '\n')
  _ <- (ignore A.newline) <|> A.eof
  pure s

strN :: Int -> A.Parser String
strN n = do
  xs <- mapM (\_ -> A.anyChar) [1..n]
  pure xs

pEither :: (a -> c) -> (A.ErrInfo -> c) -> A.Result a -> c
pEither f g r = 
    case r of
        A.Success a -> f a
        A.Failure e -> g e