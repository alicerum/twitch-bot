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

-- :igorxy69!igorxy69@igorxy69.tmi.twitch.tv PRIVMSG #androidp :Андрей сидел часок касеты перематывал и заспустил

parseMessage :: T.Text -> Maybe Message
parseMessage msg = do
    let wds = T.words msg

    if length wds > 3 && head (tail wds) == "PRIVMSG"
    then do 
        let usr = getUser (head wds)
        txt <- getMessage wds
        return $ Message usr txt
    else Nothing

getUser :: T.Text -> T.Text
getUser = T.tail . T.takeWhile (/='!')

getMessage :: [T.Text] -> Maybe T.Text
getMessage wds = if length wds > 3
                 then Just $ T.tail . T.unwords $ drop 3 wds
                 else Nothing
