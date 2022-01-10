{-# LANGUAGE OverloadedStrings #-}

module Lib (
    runTwitchClient
) where

import qualified Data.Yaml as Y
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import qualified Data.Text.IO as T
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Text (Text, pack, append)
import Network.WebSockets (Connection)
import Config (Config, token, oauth, twitch, channel, name)

runTwitchClient :: Config -> ExceptT String IO ()
runTwitchClient cfg = do
    let host = "irc-ws.chat.twitch.tv"
        port = 80
        pass = pack (token (oauth (twitch cfg)))
        oauthName = pack (name (oauth (twitch cfg)))
        chan = "#" `append` pack (channel (twitch cfg))

    lift $ withSocketsDo $ WS.runClient host port "/" (app pass oauthName chan)

    lift $ putStrLn "Config is "
    lift $ print cfg

sendCommand :: Connection -> Text -> Text -> IO ()
sendCommand conn command text = WS.sendTextData conn (command `append` " " `append` text)

app :: Text -> Text -> Text -> WS.ClientApp ()
app pass name chan conn = do
    putStrLn "Connected"

    sendCommand conn "PASS" pass
    sendCommand conn "NICK" name
    sendCommand conn "JOIN" chan

    forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg

