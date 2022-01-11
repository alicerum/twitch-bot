{-# LANGUAGE OverloadedStrings #-}

module Twitch.Bot (
    processMessage
) where

import Twitch.Message
import Data.Text (Text)
import qualified Data.Text as DT
import qualified Data.Text.IO as T

-- |username -> user's message text -> bot's response
-- Nothing if bot should ignore the user's message
type Command = Text -> Text -> Maybe Text

-- |processMessage takes user's message as an argument
-- and returns optional response. Nothing in case no response needed.
processMessage :: Message -> Maybe Text
processMessage (Message user text) =
    if not (DT.null text) && DT.head text == '!'
    then parseCommand user (DT.tail text)
    else Nothing

dispatch :: [(Text, Command)]
dispatch = [
    ("echo", echoCommand) ]

lookupCommand :: Text -> Maybe Command
lookupCommand cmd = lookup' dispatch cmd
                    where lookup' [] _ = Nothing
                          lookup' (c:others) cmd = if fst c == cmd
                                                   then Just (snd c)
                                                   else lookup' others cmd

echoCommand :: Command
echoCommand _ _ = Just "I am a haskell bot"

parseCommand :: Command
parseCommand user text = do
    let ws = DT.words text
        cmdName = if not (null ws) then head ws else ""
    cmd <- lookupCommand cmdName
    cmd user (DT.unwords (tail ws))

