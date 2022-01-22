{-# LANGUAGE OverloadedStrings #-}

module Twitch.Message (
    Message (PrivMsg, Ping),
    parseMessage,
    fromUser,
    text,
    channel
) where

import qualified Data.Text as T
import Text.Parsec.Text (Parser)
import Text.Parsec
import Text.ParserCombinators.Parsec (string, char, manyTill, anyToken, eof, space, many1, choice, parse, ParseError)
import Control.Monad (void)
import Text.Parsec.Token (GenTokenParser(whiteSpace))
import Data.Text (pack)

type Host = T.Text

data Message = Ping Host | PrivMsg {
    fromUser :: T.Text,
    channel :: T.Text,
    text :: T.Text
} deriving (Eq, Show)

pingMessage :: Parser Message
pingMessage = Ping . pack <$> (string "PING" *> many1 space
                                *> char ':' *> manyTill anyToken (string "\r\n"))

privMsgMessage :: Parser Message
privMsgMessage = do
    void $ char ':'
    nick <- manyTill anyToken (char '!')
    void $ manyTill anyToken space
    void $ string "PRIVMSG" *> space *> char '#'
    channel <- manyTill anyToken space
    void $ char ':'
    msg <- manyTill anyToken (string "\r\n")
    return $ PrivMsg (pack nick) (pack channel) (pack msg)

messageParser :: Parser Message
messageParser = choice [pingMessage, privMsgMessage]

-- |parseMessage accepts raw string input obtained from the socket
-- and returs Message object as result of raw data parse
-- Nothing in case message could not be parsed or is of no interest for us
-- at this point we only parse PRIVMSG messages here. Any other message type
-- will yield Nothing
parseMessage :: T.Text -> Either ParseError Message
parseMessage = parse messageParser ""

