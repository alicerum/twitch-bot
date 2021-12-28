module Options (
    Options,
    configPath,
    getOptions
) where

import System.Environment (getArgs)
import qualified System.Environment as E
import System.FilePath

newtype Options = Options {
    configPath :: String
}

defaultOptions :: IO Options
defaultOptions = Options . (++"/twitch-bot.yaml") . takeDirectory <$> E.getExecutablePath

parseArgs :: [String] -> Options -> Either String Options
parseArgs [] o = Right o
parseArgs (a:args) o
    | a == "-c" || a == "--config" = if not (null args)
                                     then parseArgs (tail args) o {configPath=head args}
                                     else Left "No value for '-c' argument"
    | otherwise = parseArgs args o

checkOpts :: Either String Options -> IO Options
checkOpts (Left s) = fail s
checkOpts (Right s) = return s


getOptions :: IO Options
getOptions = do
    args <- getArgs
    defaultOptions >>= checkOpts . parseArgs args

