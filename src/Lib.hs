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
import Control.Monad (forever, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Text (Text, pack, append, words)
import qualified Data.Text as DT
import Network.WebSockets (Connection)
import Config (Config, token, oauth, twitch, channel, name)
import qualified Twitch.Bot as TB
import qualified Twitch.Message as TM
import Data.Maybe

runTwitchClient :: Config -> ExceptT String IO ()
runTwitchClient cfg = do
    let host = "irc-ws.chat.twitch.tv"
        port = 80
        pass = pack (token (oauth (twitch cfg)))
        oauthName = pack (name (oauth (twitch cfg)))
        chan = "#" `append` pack (channel (twitch cfg))

    lift $ withSocketsDo $ WS.runClient host port "/" (app pass oauthName chan)


sendCommand :: Connection -> Text -> Text -> IO ()
sendCommand conn command text = WS.sendTextData conn (command `append` " " `append` text)

parsePing :: Text -> Connection -> IO ()
parsePing msg conn = do
    let parts = DT.words msg
        pongMessage = "PONG :tmi.twitch.tv"

    when (not (null parts) && head parts == "PING") $ do
        T.putStrLn ("Sending pong message: " `append` pongMessage)
        WS.sendTextData conn pongMessage

processCommand :: Text -> Connection -> IO ()
processCommand msg conn = do
    let message = TM.parseMessage msg
        result = message >>= TB.processMessage

    when (isJust result) $ do
        WS.sendTextData conn (fromJust result)

app :: Text -> Text -> Text -> WS.ClientApp ()
app pass name chan conn = do
    putStrLn "Connected"

    sendCommand conn "PASS" pass
    sendCommand conn "NICK" name
    sendCommand conn "JOIN" chan

    forever $ do
        msg <- WS.receiveData conn
        parsePing msg conn
        processCommand msg conn
        liftIO $ T.putStrLn msg

