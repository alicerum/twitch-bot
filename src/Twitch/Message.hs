{-# LANGUAGE OverloadedStrings #-}

module Twitch.Message (
    Message (Message),
    parseMessage
) where

import qualified Data.Text as T

data Message = Message {
    fromUser :: T.Text,
    text :: T.Text
} deriving (Eq, Show)

-- |parseMessage accepts raw string input obtained from the socket
-- and returs Message object as result of raw data parse
-- Nothing in case message could not be parsed or is of no interest for us
-- at this point we only parse PRIVMSG messages here. Any other message type
-- will yield Nothing
parseMessage :: T.Text -> Maybe Message
parseMessage msg = do
    let wds = T.words msg

    if length wds > 3 && head (tail wds) == "PRIVMSG"
    then do 
        let usr = getUser (head wds)
        txt <- getText wds
        return $ Message usr txt
    else Nothing

getUser :: T.Text -> T.Text
getUser = T.tail . T.takeWhile (/='!')

getText :: [T.Text] -> Maybe T.Text
getText wds = if length wds > 3
                 then Just $ T.tail . T.unwords $ drop 3 wds
                 else Nothing
