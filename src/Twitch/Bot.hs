{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Twitch.Bot (
    CommandState,
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
import qualified Twitch.Commands.Djinn.Djinn as Djinn

newtype CommandState =
    CommandState { _djinnState :: Djinn.State
    } deriving (Show)
makeLenses ''CommandState

initialState :: CommandState
initialState = CommandState Djinn.startState

-- |username -> user's message text -> bot's response
-- Nothing if bot should ignore the user's message
type Command = Text -> Text -> MaybeT IO Text
--type Command = Text -> Text -> StateT CommandState (MaybeT IO) Text

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

