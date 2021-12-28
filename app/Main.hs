module Main where

import Lib
import Options
import Config (Config(Config), Database(Database), writeConfig)

main :: IO ()
main = do
    opts <- getOptions
    putStrLn (configPath opts)

