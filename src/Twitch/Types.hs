{-#LANGUAGE TemplateHaskell #-}

module Twitch.Types (
    initialState,
    CommandState, djinnState,
    CommandWithState,
    Command
) where


import Control.Lens
import Control.Lens.TH
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.Text
import qualified Data.Map as M
import qualified Twitch.Commands.Djinn.Djinn as Djinn

type VarMap = M.Map String String

data CommandState = CommandState
    { _djinnState :: Djinn.State
    , variables :: VarMap
    } deriving (Show)
makeLenses ''CommandState

initialState :: CommandState
initialState = CommandState Djinn.startState M.empty

type CommandWithState = StateT CommandState (MaybeT IO)

-- |username -> user's message text -> bot's response
-- Nothing if bot should ignore the user's message
type Command a = Text -> Text -> CommandWithState a

