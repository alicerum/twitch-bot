module Twitch.Commands.Djinn (
    runDjinnCommand
) where

import Twitch.Commands.Djinn.HTypes
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import qualified Twitch.Commands.Djinn.Djinn as D
import Control.Monad.IO.Class (MonadIO(liftIO))


runDjinnCommand :: String -> StateT D.State (MaybeT IO) String
runDjinnCommand s = do
    state <- get
    (res, newState) <- liftIO $ D.eval state s
    put newState
    return res

