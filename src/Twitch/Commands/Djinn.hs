module Twitch.Commands.Djinn (
    runDjinnCommand
) where

import Optics
import Data.List
import Twitch.Commands.Djinn.HTypes
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import qualified Twitch.Commands.Djinn.Djinn as D
import Twitch.Types (Command, djinnState, CommandState)

runDjinnCommand :: String -> StateT CommandState (MaybeT IO) String
runDjinnCommand s = do
    state <- get
    (res, newState) <- lift $ MaybeT $ Just <$> D.eval (state ^. djinnState) s
    put $ state & djinnState .~ newState
    return (intercalate " | " . lines $ res)

