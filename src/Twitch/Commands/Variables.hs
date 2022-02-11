{-# LANGUAGE OverloadedStrings #-}

module Twitch.Commands.Variables
    ( letCommand
    , unletCommand
    , clearCommand
) where

import Data.Text
import Twitch.Types
import Text.Parsec
import Text.Parsec.Text

bindSymbol :: Parser Char
bindSymbol = letter <|> digit <|> char '\'' <|> char '_'
                <|> char '-'

bindName :: Parser Text
bindName = pack <$> ((:) <$> letter <*> many bindSymbol)

delim :: Parser Char
delim = spaces *> char '=' <* spaces

expr :: Parser Text
expr = pack <$> manyTill anyChar eof

parseCommand :: Parser (Text, Text)
parseCommand = (,) <$> bindName <* delim <*> expr

letCommand :: Command Text
letCommand user command = do
    return "fff"

unletCommand :: Command Text
unletCommand user command = do
    return "fff"

clearCommand :: Command Text
clearCommand user command = do
    return "fff"

