{-# LANGUAGE OverloadedStrings #-}

module Twitch.Bot (
    processMessage
) where

import Twitch.Message
import Control.Monad (guard)
import Data.Text (Text)
import qualified Data.Text as DT
import qualified Data.Text.IO as T

-- |username -> user's message text -> bot's response
-- Nothing if bot should ignore the user's message
type Command = Text -> Text -> Maybe Text

-- |processMessage takes user's message as an argument
-- and returns optional response. Nothing in case no response needed.
processMessage :: Message -> Maybe Text
processMessage (Message user text) = do
    guard $ not (DT.null text)
    guard $ DT.head text == '!'
    parseCommand user (DT.tail text)

parseCommand :: Command
parseCommand user text = do
    let ws = DT.words text
        cmdName = if not (null ws) then head ws else ""
    cmd <- lookup cmdName dispatch
    cmd user (DT.unwords (tail ws))

dispatch :: [(Text, Command)]
dispatch = [
    ("echo", echoCommand) ]

echoCommand :: Command
echoCommand _ _ = Just "I am a haskell bot"

