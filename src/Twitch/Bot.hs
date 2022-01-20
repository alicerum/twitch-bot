{-# LANGUAGE OverloadedStrings #-}

module Twitch.Bot (
    processMessage,
    hoistMaybe
) where

import Twitch.Message
import Control.Monad (guard)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as DT
import qualified Data.Text.IO as T
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Twitch.Commands.Runh (runHString)

-- |username -> user's message text -> bot's response
-- Nothing if bot should ignore the user's message
type Command = Text -> Text -> MaybeT IO Text

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return

-- |processMessage takes user's message as an argument
-- and returns optional response. Nothing in case no response needed.
processMessage :: Message -> MaybeT IO Text
processMessage (Ping _) = hoistMaybe Nothing
processMessage (PrivMsg user _ text) = do
    guard $ not (DT.null text)
    guard $ DT.head text == '!'
    parseCommand user (DT.tail text)

parseCommand :: Command
parseCommand user text = do
    let ws = DT.words text
        cmdName = if not (null ws) then head ws else ""
    cmd <- hoistMaybe $ lookup cmdName dispatch
    cmd user (DT.unwords (tail ws))

dispatch :: [(Text, Command)]
dispatch = [
    ("echo", echoCommand),
    ("runh", runhCommand), 
    ("boroda", echoBoroda) ]

echoCommand :: Command
echoCommand _ _ = return "I am a haskell bot"

runhCommand :: Command
runhCommand user command = do
    str <- lift $ runHString (unpack command)
    return $ "@" <> user <> " > " <> pack str

echoBoroda :: Command
echoBoroda user _ = return $ user <> " says HI to @LzheBoroda via haskell bot"

