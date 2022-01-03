module Main where

import Lib
import Options
import Config (Config, writeDefaultConfig, readConfig)
import System.IO (hPutStrLn, stderr)
import Data.Yaml (prettyPrintParseException, ParseException)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

data TwitchError = OptionsError String | ParserError ParseException

transformException :: ParseException -> String
transformException = ("Error during config parse: "++) . prettyPrintParseException

processConfig :: Options -> ExceptT String IO ()
processConfig opts = do
    c <- withExceptT transformException (ExceptT (readConfig (configPath opts)))
    lift $ putStrLn "Config is:"
    lift $ print c


processOptions :: ExceptT String IO ()
processOptions = do
    opts <- ExceptT getOptions
    if help opts
    then lift showHelp
    else
        if newConfig opts
        then lift $ do
            let path = configPath opts
            putStrLn $ "Creating default config: " ++ path
            writeDefaultConfig path
        else processConfig opts

main :: IO ()
main = do
    result <- runExceptT processOptions
    case result of
        Left msg -> hPutStrLn stderr msg
        Right () -> return ()

