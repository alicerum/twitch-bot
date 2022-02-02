module Main where

import Lib
import Optics
import Options
import Config (Config, writeDefaultConfig, readConfig)
import System.IO (hPutStrLn, stderr)
import Data.Yaml (prettyPrintParseException, ParseException)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import System.Exit (exitWith, ExitCode (ExitFailure))

data TwitchError = OptionsError String | ParserError ParseException

transformException :: ParseException -> String
transformException = ("Error during config parse: "++) . prettyPrintParseException

processConfig :: Options -> ExceptT String IO ()
processConfig opts = do
    c <- withExceptT transformException (ExceptT (readConfig (opts ^. configPath)))
    runTwitchClient c

processOptions :: ExceptT String IO ()
processOptions = do
    opts <- ExceptT getOptions
    checkOpts opts
    where
        checkOpts opts
            | opts ^. help = lift showHelp
            | opts ^. newConfig = lift $ createNewConfig $ opts ^. configPath
            | otherwise = processConfig opts

createNewConfig :: FilePath -> IO ()
createNewConfig path = do
    putStrLn $ "Creating default config at " ++ path
    writeDefaultConfig path

exitWithMessage :: String -> IO ()
exitWithMessage errMsg = hPutStrLn stderr errMsg >> exitWith (ExitFailure 1)

main :: IO ()
main = do
    result <- runExceptT processOptions
    case result of
        Left msg -> exitWithMessage msg
        Right () -> return ()

