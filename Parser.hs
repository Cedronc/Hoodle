{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Text.Parsec
import qualified Text.Parsec
import Text.Parsec.String (Parser)
-- import Control.Applicative ((<|>), many, some)
import Data.Char (isAlphaNum)
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Monad.Except
import Pool
import qualified LazyHoodle as L
import Data.Time.LocalTime (LocalTime)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

-- Monads
type AppState = (TVar UserDB, TVar (MapPool Int (L.LazyHoodle LocalTime)))
type AppMonad a = ReaderT AppState (ExceptT Response STM) a

runApp :: AppMonad a -> AppState -> STM (Either Response a)
runApp app state = runExceptT (runReaderT app state)

runAppAtomically :: AppMonad a -> AppState -> IO (Either Response a)
runAppAtomically app state = atomically (runApp app state)

--  TYPES

type Token      = String
type Login      = (Token, Token)
type Time       = LocalTime
type Slot       = L.Timeslot Time
type Slots      = [Slot]
type PHoodle = L.LazyHoodle Time
type Hoodles    = MapPool Int PHoodle
type Schedule   = Hoodles

data Request
  = AddUser Login Token
  | GetHoodle Token
  | ChangePassword Login Token
  | AddHoodle Login Token PHoodle
  | EditHoodle Login Token PHoodle
  | RemoveHoodle Login Token
  | Register Login Token
  | Unregister Login Token
  | Toggle Login Token Char
  | OptimalSchedule Login
  deriving (Show)

data Response
  = WrongLogin
  | OkToken Token
  | OkHoodle PHoodle
  | OkSchedule Schedule
  | NotPermitted
  | InvalidHoodle
  | NoSuchId
  | NoSuchSlot
  | NotRegistered
  | AlreadyRegistered
  | NoPossibleSchedule
  deriving (Show)

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
timeToken = lexeme $ read <$> many1 (alphaNum <|> char ':' <|> char '-' <|> char '+' <|> char 'T')

parseInt :: Parser Int
parseInt = lexeme $ read <$> many1 digit

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
parseSlot = (\s e -> L.Timeslot s e $ Set.fromList []) <$> timeToken <*> (slash *> timeToken)

parseSlots :: Parser Slots
parseSlots = parseSlot `sepEndBy` comma

parseHoodle :: Parser PHoodle
parseHoodle = do
  name <- parseToken          -- Parse name (String)
  _    <- spaces              -- Skip whitespace
  ts   <- lbrack *> parseSlots <* rbrack  -- Parse timeslots: [start/end, ...]
  return $ L.LazyHoodle name (Seq.fromList ts) (Set.fromList [])
  where
    -- Parse comma-separated attendees (e.g., "alice,bob,charlie")
    parseAttendees :: Parser [String]
    parseAttendees = parseToken `sepBy` comma

-- These fuckheads aren't needed i think so I won't fix them from the first draft versions
-- parseHoodlesEntry :: Parser Hoodles
-- parseHoodlesEntry = (,) <$> parseToken <*> (colon *> parseSlot)
-- parseHoodles :: Parser Hoodles
-- parseHoodles = parseHoodlesEntry `sepEndBy` comma
-- parseSchedule :: Parser Schedule
-- parseSchedule = lcurly *> parseHoodles <* rcurly

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
  , try (Toggle <$ stringL "toggle" <*> parseLogin <*> parseToken <*> digit)
  , try (OptimalSchedule <$ stringL "optimal-schedule" <*> parseLogin)
  ]

-- TOP-LEVEL PARSER

parseProtocolMessage :: String -> Either ParseError Request
parseProtocolMessage input =
  case parse parseRequest "<input>" input of
    Right r -> Right r
    Left err -> Left err


-- Users

-- | A database mapping usernames (Token) to passwords (Token)
type UserDB = Map.Map Token Token

emptyDB :: UserDB
emptyDB = Map.empty

addUser :: Token -> Token -> UserDB -> UserDB
addUser = Map.insert

validLogin :: Login -> UserDB -> Bool
validLogin (username, password) db =
  Map.lookup username db == Just password

changePassword :: Login -> Token -> AppMonad Response
changePassword login newPass = do
  (tvarDB, _) <- ask
  -- first lifted to (ExceptT Response STM) then lifted to (ReaderT AppState)
  db <- lift . lift $ readTVar tvarDB 
  if validLogin login db
    then return (OkToken "changed")
    else throwError WrongLogin
