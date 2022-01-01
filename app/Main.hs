module Main where

import Lib
import Options
import Config (Config(Config), Database(Database), writeDefaultConfig, readConfig)

main :: IO ()
main = do
    opts <- getOptions
    if help opts
    then do showHelp
    else do
        if newConfig opts
        then do
            let path = configPath opts
            putStrLn $ "Creating default config: " ++ path
            writeDefaultConfig path
        else do
            c <- readConfig (configPath opts)
            print c

