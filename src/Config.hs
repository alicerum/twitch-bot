{-# LANGUAGE OverloadedStrings #-}

module Config (
    Config (twitch),
    Twitch (oauth, channel),
    Oauth (token, name),
    Database,
    writeDefaultConfig,
    readConfig,
) where

import System.IO (withFile, IOMode (ReadMode, WriteMode), hPutStr)
import Data.Yaml
import System.Directory (createDirectoryIfMissing)
import qualified Data.ByteString as B
import qualified Data.Text.Lazy.IO as DTL
import System.Environment (lookupEnv, getEnv)
import Data.Maybe (fromMaybe)

data Config = Config {
    twitch :: Twitch
  , database :: Database
  } deriving (Eq, Show)

data Oauth = Oauth {
      token :: String
    , name :: String
} deriving (Eq, Show)

data Twitch = Twitch {
      oauth :: Oauth
    , channel :: String
} deriving (Eq, Show)

data Database = Database {
      path :: FilePath
    , username :: String
    , password :: String
    } deriving (Eq, Show)

instance ToJSON Config where
    toJSON config =
        let db = database config
            twt = twitch config
            twitchOauth = oauth twt

        in object [
            "twitch" .= object [
                "channel" .= channel twt,
                "oauth" .= object [
                    "token" .= token twitchOauth,
                    "name" .= name twitchOauth
                ]
            ],
            "database" .= object [
                "path" .= path db,
                "username" .= username db,
                "password" .= password db ] ]

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> do
        twitchValue <- o .: "twitch"
        twitchChannel <- twitchValue .: "channel"

        oauthValue <- twitchValue .: "oauth"
        oauthToken <- oauthValue .: "token"
        oauthName <- oauthValue .: "name"

        dbValue <- o .: "database"
        path <- dbValue .: "path"
        username <- dbValue .: "username"
        password <- dbValue .: "password"

        return $ Config (Twitch
                            (Oauth oauthToken oauthName) twitchChannel)
                            (Database path username password)


defaultConfig :: Config
defaultConfig = Config (Twitch
                            (Oauth "" "") "")
                            (Database "" "" "")

writeDefaultConfig :: FilePath -> IO ()
writeDefaultConfig path = do
    withFile path WriteMode (\handle -> do
        B.hPutStr handle (encode defaultConfig))

readConfig :: FilePath -> IO (Either ParseException Config)
readConfig path = do
    withFile path ReadMode (\handle -> do
        contents <- B.hGetContents handle
        return $ decodeEither' contents)

