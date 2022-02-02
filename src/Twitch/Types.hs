{-#LANGUAGE TemplateHaskell #-}

module Twitch.Types (
    initialState,
    CommandState, djinnState,
    CommandWithState,
    Command
) where


import Optics.Lens
import Optics.TH
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.Text
import qualified Twitch.Commands.Djinn.Djinn as Djinn

newtype CommandState =
    CommandState { _djinnState :: Djinn.State
    } deriving (Show)
makeLenses ''CommandState

initialState :: CommandState
initialState = CommandState Djinn.startState

type CommandWithState = StateT CommandState (MaybeT IO)
-- |username -> user's message text -> bot's response
-- Nothing if bot should ignore the user's message
type Command a = Text -> Text -> CommandWithState a

