{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Config (
    Config, twitch, database,
    Twitch, oauth, channel,
    Oauth, token, name,
    writeDefaultConfig,
    readConfig,
) where

import GHC.Generics
import Control.Lens
import System.IO (withFile, IOMode (ReadMode, WriteMode), hPutStr)
import Data.Yaml
import Data.Aeson.TH
import Data.Aeson.Types
import System.Directory (createDirectoryIfMissing)
import qualified Data.ByteString as B
import qualified Data.Text.Lazy.IO as DTL
import System.Environment (lookupEnv, getEnv)
import Data.Maybe (fromMaybe)

data Config = Config {
    _twitch :: Twitch
  , _database :: Database
  } deriving (Eq, Show, Generic)

data Oauth = Oauth {
      _token :: String
    , _name :: String
} deriving (Eq, Show, Generic)

data Twitch = Twitch {
      _oauth :: Oauth
    , _channel :: String
} deriving (Eq, Show, Generic)

data Database = Database {
      _path :: FilePath
    , _username :: String
    , _password :: String
} deriving (Eq, Show, Generic)

makeLenses ''Config
makeLenses ''Oauth
makeLenses ''Twitch
makeLenses ''Database

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Config
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Oauth
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Twitch
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Database


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

