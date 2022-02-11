{-# LANGUAGE OverloadedStrings #-}

module Twitch.Bot (
    CommandState,
    CommandWithState,
    initialState,
    processMessage,
    hoistMaybe
) where

import Control.Lens
import Twitch.Message
import Control.Monad (guard)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as DT
import qualified Data.Text.IO as T
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Twitch.Commands.Runh (runHString)
import Control.Monad.Trans.State
import Twitch.Types
import qualified Twitch.Commands.Variables as TV
import qualified Twitch.Commands.Djinn.Djinn as Djinn
import Twitch.Commands.Djinn (runDjinnCommand)


hoistMaybe :: Maybe a -> CommandWithState a
hoistMaybe = lift . MaybeT . return

-- |processMessage takes user's message as an argument
-- and returns optional response. Nothing in case no response needed.
processMessage :: Message -> CommandWithState Text
processMessage (Ping _) = hoistMaybe Nothing
processMessage (PrivMsg user _ text) = do
    guard $ not (DT.null text)
    guard $ DT.head text == '!'
    parseCommand user (DT.tail text)

parseCommand :: Command Text
parseCommand user text = do
    let ws = DT.words text
        cmdName = if not (null ws) then head ws else ""
    cmd <- hoistMaybe $ lookup cmdName dispatch
    cmd user (DT.unwords (tail ws))

dispatch :: [(Text, Command Text)]
dispatch = [
    ("echo",   echoCommand),
    ("runh",   runhCommand), 
    ("djinn",  djinnCommand),
    ("let",    TV.letCommand),
    ("unlet",  TV.unletCommand),
    ("clear",  TV.clearCommand),
    ("boroda", echoBoroda) ]

echoCommand :: Command Text
echoCommand _ _ = return "I am a haskell bot"

runhCommand :: Command Text
runhCommand user command = do
    str <- runHString (unpack command)
    return $ "@" <> user <> " > " <> pack str

djinnCommand :: Command Text
djinnCommand user msg = pack . take 400 <$> runDjinnCommand (unpack msg)

echoBoroda :: Command Text
echoBoroda user _ = return $ "@" <> user <> " says HI to @LzheBoroda via haskell bot"

