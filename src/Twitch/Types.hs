{-#LANGUAGE TemplateHaskell #-}

module Twitch.Types (
    VarList, VarName, VarDef,
    initialState,
    CommandState, djinnState, variables,
    CommandWithState,
    Command
) where


import Optics.Lens
import Optics.TH
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.Text
import qualified Data.Map as M
import qualified Twitch.Commands.Djinn.Djinn as Djinn

type VarName = Text
type VarDef = Text
type VarList = [(VarName, VarDef)]

data CommandState = CommandState
    { _djinnState :: Djinn.State
    , _variables :: VarList
    } deriving (Show)
makeLenses ''CommandState

initialState :: CommandState
initialState = CommandState Djinn.startState []

type CommandWithState = StateT CommandState (MaybeT IO)

-- |username -> user's message text -> bot's response
-- Nothing if bot should ignore the user's message
type Command a = Text -> Text -> CommandWithState a

