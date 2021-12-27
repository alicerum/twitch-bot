{-# LANGUAGE OverloadedStrings #-}

module Config (
    Config(Config),
    oauthToken,
    dbPath,
    writeConfig
) where

import System.IO (withFile, IOMode (ReadMode, WriteMode), hPutStr)
import Data.Yaml
import System.Directory (createDirectoryIfMissing)
import qualified Data.ByteString as B
import qualified Data.Text.Lazy.IO as DTL
import System.Environment (lookupEnv, getEnv)
import Data.Maybe (fromMaybe)

data Config = Config {
    oauthToken :: String
  , dbPath :: FilePath
  } deriving (Eq, Show)

instance ToJSON Config where
    toJSON config = object [
        "oauthToken" .= oauthToken config,
        "dbPath" .= dbPath config ]

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> Config
        <$> o .: "oauthToken"
        <*> o .: "dbPath"


getConfigFilePath :: IO FilePath
getConfigFilePath = do
    result  <- lookupEnv "XDG_CONFIG_HOME"
    home <- getEnv "HOME"
    let xdgHome = fromMaybe (home ++ "/.config/") result
        dir = xdgHome ++ "twitch-bot/"
        filename = dir ++ "config.yaml"

    createDirectoryIfMissing True dir
    return filename


writeConfig :: Config -> IO ()
writeConfig config = do
    filename <- getConfigFilePath

    withFile filename WriteMode (\handle -> do
        B.hPutStr handle (encode config))

