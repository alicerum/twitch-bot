{-# LANGUAGE OverloadedStrings #-}

module Lib (
    runTwitchClient
) where

import qualified Data.Yaml as Y
import Config (Config, oauthName, oauthToken)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import qualified Data.Text.IO as T
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Text (Text, pack, append)
import Network.WebSockets (Connection)

runTwitchClient :: Config -> ExceptT String IO ()
runTwitchClient cfg = do
    let host = "irc-ws.chat.twitch.tv"
        port = 80
        token = oauthToken cfg
        name = oauthName cfg

    lift $ withSocketsDo $ WS.runClient host port "/" (app (pack token) (pack name))

    lift $ putStrLn "Config is "
    lift $ print cfg

sendCommand :: Connection -> Text -> Text -> IO ()
sendCommand conn command text = WS.sendTextData conn (command `append` " " `append` text)

app :: Text -> Text -> WS.ClientApp ()
app pass name conn = do
    putStrLn "Connected"

    sendCommand conn "PASS" pass
    sendCommand conn "NICK" name

    forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg

