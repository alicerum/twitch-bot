module Main where

import Lib
import Config (Config(Config), oauthToken, dbPath, writeConfig)

main :: IO ()
main = do
    let config = Config { oauthToken = "hahaha", dbPath = "/home/wv/db.sqlite" }
    writeConfig config

