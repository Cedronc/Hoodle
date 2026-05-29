{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Text.Parsec
import qualified Text.Parsec
import Text.Parsec.String (Parser)
-- import Control.Applicative ((<|>), many, some)
import Data.Char (isAlphaNum)

--  TYPES

type Token    = String
type Login    = (Token, Token)  -- LOGIN → TOKEN : TOKEN
type Time     = String
type Slot     = (Time, Time)    -- SLOT → TIME / TIME
type Slots    = [Slot]
type Hoodle   = Slots           -- HOODLE → [ SLOTS ]
type Hoodles  = [(Token, Slot)] -- HOODLES → TOKEN : SLOT , HOODLES
type Schedule = Hoodles         -- SCHEDULE → { HOODLES }

data Request
  = AddUser Login Token
  | GetHoodle Token
  | ChangePassword Login Token
  | AddHoodle Login Token Hoodle
  | EditHoodle Login Token Hoodle
  | RemoveHoodle Login Token
  | Register Login Token
  | Unregister Login Token
  | Toggle Login Token Slot
  | OptimalSchedule Login
  deriving (Show, Eq)

data Response
  = WrongLogin
  | OkToken Token
  | OkHoodle Hoodle
  | OkSchedule Schedule
  | NotPermitted
  | InvalidHoodle
  | NoSuchId
  | NoSuchSlot
  | NotRegistered
  | AlreadyRegistered
  | NoPossibleSchedule
  deriving (Show, Eq)

-- LEXER HELPERS

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

symbol :: Char -> Parser Char
symbol c = lexeme (char c)

stringL :: String -> Parser String
stringL s = lexeme (string s)

parseToken :: Parser String
parseToken = lexeme $ many1 (alphaNum <|> char '-' <|> char '_')

timeToken :: Parser Time
timeToken = lexeme $ many1 (alphaNum <|> char ':' <|> char '-' <|> char '+' <|> char 'T')

-- Fix all single-character parsers:
lbrack, rbrack, lcurly, rcurly, comma, colon, slash :: Parser Char
lbrack = symbol '['
rbrack = symbol ']'
lcurly = symbol '{'
rcurly = symbol '}'
comma  = symbol ','
colon  = symbol ':'
slash  = symbol '/'

-- BASIC PARSERS
-- `sepEndBy` allows trailing parsing thingies

parseLogin :: Parser Login
parseLogin = (,) <$> parseToken <*> (colon *> parseToken)

parseSlot :: Parser Slot
parseSlot = (,) <$> timeToken <*> (slash *> timeToken)

parseSlots :: Parser Slots
parseSlots = parseSlot `sepEndBy` comma

parseHoodle :: Parser Hoodle
parseHoodle = lbrack *> parseSlots <* rbrack

parseHoodlesEntry :: Parser (Token, Slot)
parseHoodlesEntry = (,) <$> parseToken <*> (colon *> parseSlot)

parseHoodles :: Parser Hoodles
parseHoodles = parseHoodlesEntry `sepEndBy` comma

parseSchedule :: Parser Schedule
parseSchedule = lcurly *> parseHoodles <* rcurly

-- REQUEST PARSER

parseRequest :: Parser Request

parseRequest = choice
  [ try (AddUser <$ stringL "add-user" <*> parseLogin <*> parseToken)
  , try (AddHoodle <$ stringL "add-hoodle" <*> parseLogin <*> parseToken <*> parseHoodle)
  , try (ChangePassword <$ stringL "change-password" <*> parseLogin <*> parseToken)
  , try (EditHoodle <$ stringL "edit-hoodle" <*> parseLogin <*> parseToken <*> parseHoodle)
  , try (RemoveHoodle <$ stringL "remove-hoodle" <*> parseLogin <*> parseToken)
  , try (GetHoodle <$ stringL "get-hoodle" <*> parseToken)
  , try (Register <$ stringL "register" <*> parseLogin <*> parseToken)
  , try (Unregister <$ stringL "unregister" <*> parseLogin <*> parseToken)
  , try (Toggle <$ stringL "toggle" <*> parseLogin <*> parseToken <*> parseSlot)
  , try (OptimalSchedule <$ stringL "optimal-schedule" <*> parseLogin)
  ]

-- TOP-LEVEL PARSER

parseProtocolMessage :: String -> Either ParseError Request
parseProtocolMessage input =
  case parse parseRequest "<input>" input of
    Right r -> Right r
    Left err -> Left err
