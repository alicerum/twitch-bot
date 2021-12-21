{-# LANGUAGE OverloadedStrings #-}

module Config (
    Config(Config),
    oauthToken,
    dbPath,
    writeConfig
) where

import System.IO (withFile, IOMode (ReadMode, WriteMode), hPutStr)
import Data.Yaml
import qualified Data.ByteString as B
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

writeConfig :: Config -> IO ()
writeConfig config = do
    result  <- lookupEnv "XDG_CONFIG_HOME"
    home <- getEnv "HOME"
    let xdgHome = fromMaybe (home ++ "/.config/") result

    putStrLn $ "XDG_CONFIG_HOME is: " ++ xdgHome

    withFile "/Users/wyvie/test.yaml" WriteMode (\handle -> do
        B.hPutStr handle (encode config))

