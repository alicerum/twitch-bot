module Options (
    Options,
    configPath,
    newConfig,
    help,
    getOptions,
    showHelp
) where

import System.Environment (getArgs)
import qualified System.Environment as E
import System.FilePath

data Options = Options {
    configPath :: String,
    help :: Bool,
    newConfig :: Bool
} deriving (Show)

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
                                     then parseArgs (tail args) o {configPath=head args}
                                     else Left "No value for '-c' argument"
    | a == "-n" || a == "--new" = parseArgs args o {newConfig=True}
    | a == "-h" || a == "--help" = parseArgs args o {help=True}
    | otherwise = parseArgs args o

checkOpts :: Either String Options -> IO Options
checkOpts (Left s) = fail s
checkOpts (Right s) = return s


getOptions :: IO (Either String Options)
getOptions = do
    args <- getArgs
    parseArgs args <$> defaultOptions

