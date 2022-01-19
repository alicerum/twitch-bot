{-# LANGUAGE OverloadedStrings #-}

module Lib (
    runTwitchClient
) where

import qualified Wuss as WSS
import qualified Data.Yaml as Y
import Data.Either.Combinators
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import qualified Data.Text.IO as T
import Control.Monad (forever, when, (>=>), forM_)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Text (Text, pack, append, words)
import qualified Data.Text as DT
import qualified Data.Text.IO as DTIO
import Network.WebSockets (Connection)
import Config (Config, token, oauth, twitch, channel, name)
import qualified Twitch.Bot as TB
import qualified Twitch.Message as TM
import Data.Maybe
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)

runTwitchClient :: Config -> ExceptT String IO ()
runTwitchClient cfg = do
    let host = "irc-ws.chat.twitch.tv"
        port = 443
        pass = pack (token (oauth (twitch cfg)))
        oauthName = pack (name (oauth (twitch cfg)))
        chan = "#" `append` pack (channel (twitch cfg))

    lift $ withSocketsDo $ WSS.runSecureClient host port "/" (app pass oauthName chan)


sendCommand :: Connection -> Text -> Text -> IO ()
sendCommand conn command text = WS.sendTextData conn (command `append` " " `append` text)

processMessage :: Text -> TM.Message -> MaybeT IO Text
processMessage _ (TM.Ping host) = return $ "PONG :" <> host
processMessage chan msg@TM.PrivMsg{} = TB.processMessage msg >>= \resp -> return $ "PRIVMSG " <> chan <> " :" <> resp

processCommand :: Text -> Text -> Connection -> IO ()
processCommand msg chan conn = do
    let message = TM.parseMessage msg
    print message
    response <- runMaybeT $ TB.hoistMaybe (rightToMaybe message) >>= processMessage chan
    forM_ response $ WS.sendTextData conn

app :: Text -> Text -> Text -> WS.ClientApp ()
app pass name chan conn = do
    putStrLn "Connected"

    sendCommand conn "PASS" pass
    sendCommand conn "NICK" name
    sendCommand conn "JOIN" chan

    forever $ do
        msg <- WS.receiveData conn
        processCommand msg chan conn

