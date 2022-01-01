{-# LANGUAGE OverloadedStrings #-}

module Config (
    Config(Config),
    Database(Database),
    writeDefaultConfig,
    readConfig
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
  , database :: Database
  } deriving (Eq, Show)

data Database = Database {
      path :: FilePath
    , username :: String
    , password :: String
    } deriving (Eq, Show)

instance ToJSON Config where
    toJSON config =
        let db = database config
        in object [
            "oauthToken" .= oauthToken config,
            "database" .= object [
                "path" .= path db,
                "username" .= username db,
                "password" .= password db ] ]

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> do
        oauthToken <- o .: "oauthToken"
        dbValue <- o .: "database"
        path <- dbValue .: "path"
        username <- dbValue .: "username"
        password <- dbValue .: "password"

        return $ Config oauthToken (Database path username password)


getConfigFilePath :: IO FilePath
getConfigFilePath = do
    result  <- lookupEnv "XDG_CONFIG_HOME"
    home <- getEnv "HOME"
    let xdgHome = fromMaybe (home ++ "/.config/") result
        dir = xdgHome ++ "twitch-bot/"
        filename = dir ++ "config.yaml"

    createDirectoryIfMissing True dir
    return filename

defaultConfig :: Config
defaultConfig = Config "" (Database "" "" "")

writeDefaultConfig :: FilePath -> IO ()
writeDefaultConfig path = do
    withFile path WriteMode (\handle -> do
        B.hPutStr handle (encode defaultConfig))

decodeConfig :: FilePath -> B.ByteString -> IO Config
decodeConfig path bs = case decodeEither' bs of
    Right c -> return c
    Left e -> fail $ "Could not read config from config file: " ++ prettyPrintParseException e

readConfig :: FilePath -> IO Config
readConfig path = do
    withFile path ReadMode (\handle -> do
        contents <- B.hGetContents handle
        decodeConfig path contents)

