{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative (liftA2)
import Data.Char
import Data.Maybe

data ParserRes a = Yes String a | No String
  deriving (Functor, Show)

newtype Parser a = Parser
  {runParser :: String -> ParserRes a}
  deriving (Functor)

instance Applicative Parser where
  pure a = Parser $ \s -> Yes s a
  pf <*> pa = Parser $ \s -> case runParser pf s of
    Yes s' f -> f <$> runParser pa s'
    No s' -> No s'

instance Monad Parser where
  pa >>= f = Parser $ \s -> case runParser pa s of
    Yes s' x -> runParser (f x) s'
    No s' -> No s'

run p s = case runParser p s of
  Yes _ x -> Just x
  No _ -> Nothing

parseNo = Parser No

try p = Parser $ \s -> case runParser p s of
  No _ -> No s
  success -> success

tryMaybe p = Parser $ \s -> case runParser p s of
  Yes s x -> Yes s $ Just x
  No s -> Yes s Nothing

satisfy predicate = try $ do
  x <- anything
  if predicate x
    then pure x
    else parseNo

p1 <|> p2 = Parser $ \s -> case runParser p1 s of
  No _ -> runParser p2 s
  success -> success

many p = many1 p <|> pure []

many1 p = liftA2 (:) p $ many p

choice :: [Parser a] -> Parser a
choice = foldr (<|>) parseNo

char c = satisfy (== c)

digit = satisfy isDigit

space = satisfy isSpace

string :: String -> Parser String
string = traverse char

digits = many1 digit

pToInt :: Parser (String -> Int)
pToInt = pure read

anything = Parser $ \case
  (x : xs) -> Yes xs x
  [] -> No ""

digit19 = satisfy (`elem` ['1' .. '9'])

jsonNumber = do
  sign <- tryMaybe $ char '-'
  a <- liftA2 (:) digit19 $ many digit <|> string "0"
  decimal <- tryMaybe $ liftA2 (:) (char '.') digits
  e <- tryMaybe $ do
    e <- char 'e' <|> char 'E'
    sign <- tryMaybe $ char '+' <|> char '-'
    digits <- digits
    pure $ e : (catMaybes [sign] ++ digits)
  pure $ catMaybes [sign] ++ a ++ fromMaybe "" decimal ++ fromMaybe "" e
