{-# LANGUAGE OverloadedStrings #-}

module Lib (
    runTwitchClient
) where

import Optics
import Control.Monad
import qualified Wuss as WSS
import Data.Either.Combinators
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import qualified Data.Text.IO as T
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Text (Text, pack, append, words)
import qualified Data.Text as DT
import qualified Data.Text.IO as DTIO
import Network.WebSockets (Connection)
import Config
import qualified Twitch.Bot as TB
import qualified Twitch.Message as TM
import Data.Maybe
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Control.Monad.State (StateT (runStateT), MonadState (get, put))
import Control.Concurrent (newChan, Chan, threadDelay, readChan, writeChan, forkIO)

runTwitchClient :: Config -> ExceptT String IO ()
runTwitchClient cfg = do
    let host = "irc-ws.chat.twitch.tv"
        port = 443
        pass = pack (cfg ^. twitch % oauth % token)
        oauthName = pack (cfg ^. twitch % oauth % name)
        chan = "#" <> pack (cfg ^. twitch % channel)

    lift $ withSocketsDo $ WSS.runSecureClient host port "/" (app pass oauthName chan)

sendResponseMsg :: Chan Text -> Text -> IO ()
sendResponseMsg msgChan text = do
    writeChan msgChan text

sendCommand :: Connection -> Text -> Text -> IO ()
sendCommand conn command text = WS.sendTextData conn (command <> " " <> text)

processMessage :: Text -> TM.Message -> StateT TB.CommandState (MaybeT IO) Text
processMessage _ (TM.Ping host) = return $ "PONG :" <> host
processMessage chan msg@TM.PrivMsg{} = do
    resp <- TB.processMessage msg
    return $ "PRIVMSG " <> chan <> " :" <> resp

printMessage :: TM.Message -> IO ()
printMessage (TM.Ping host) = T.putStrLn $ "PING from " <> host
printMessage (TM.PrivMsg user chan msg) = T.putStrLn $ "#" <> chan <> "> " <> user <> ": " <> msg

processCommand :: Chan Text -> Text -> Text -> Connection -> StateT TB.CommandState IO ()
processCommand msgChan msg chan conn = do
    let message = TM.parseMessage msg
    forM_ message $ lift . printMessage

    forM_ (rightToMaybe  message) $ \message -> do
        state <- get
        let maybeT = runStateT (processMessage chan message) state
            mio = runMaybeT maybeT
        res <- lift mio
        forM_ res $ \(text, newS) -> do
            put newS
            lift $ sendResponseMsg msgChan text

loop :: Chan Text -> Text -> Connection -> StateT TB.CommandState IO ()
loop msgChan chan conn = do
    msg <- lift $ WS.receiveData conn
    processCommand msgChan msg chan conn
    loop msgChan chan conn

sendMsgLoop :: Chan Text -> Connection -> IO ()
sendMsgLoop msgChan conn = do
    text <- readChan msgChan
    WS.sendTextData conn text
    threadDelay $ 3 * (10^6) -- 3 seconds, time for twitch to wait until send
    sendMsgLoop msgChan conn

app :: Text -> Text -> Text -> WS.ClientApp ()
app pass name chan conn = do
    putStrLn "Connected"

    sendCommand conn "PASS" pass
    sendCommand conn "NICK" name
    sendCommand conn "JOIN" chan

    msgChannel <- newChan
    -- run messageloop with delay in another thread
    forkIO $ sendMsgLoop msgChannel conn

    runStateT (loop msgChannel chan conn) TB.initialState
    return ()

