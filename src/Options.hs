{-# LANGUAGE TemplateHaskell #-}

module Options (
    Options(Options),
    configPath,
    newConfig,
    help,
    getOptions,
    showHelp
) where

import System.Environment (getArgs)
import qualified System.Environment as E
import System.FilePath
import Control.Lens

data Options = Options {
    _configPath :: String,
    _help :: Bool,
    _newConfig :: Bool
} deriving (Show)
makeLenses ''Options

showHelp :: IO()
showHelp = do
    putStrLn "Help Stub"

defaultOptions :: IO Options
defaultOptions = Options <$> ((++"/twitch-bot.yaml") . takeDirectory <$> E.getExecutablePath)
                         <*> pure False
                         <*> pure False

parseArgs :: [String] -> Options -> Either String Options
parseArgs [] o = Right o
parseArgs (a:args) o
    | a == "-c" || a == "--config" = if not (null args) && head (head args) /= '-'
                                     then parseArgs (tail args) $ o & configPath .~ head args
                                     else Left "No value for '-c' argument"
    | a == "-n" || a == "--new" = parseArgs args $ o & newConfig .~ True
    | a == "-h" || a == "--help" = parseArgs args $ o & help .~ True
    | otherwise = parseArgs args o

getOptions :: IO (Either String Options)
getOptions = do
    args <- getArgs
    parseArgs args <$> defaultOptions

