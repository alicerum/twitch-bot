module Twitch.Commands.Runh (
    runHString
) where

import Control.Monad (join)
import Control.Concurrent
import Control.Applicative ((<|>))
import System.Directory.Internal.Prelude (timeout, fromMaybe)
import Language.Haskell.Interpreter (as, runInterpreter, setImports, interpret)
import Control.Exception (evaluate)
import Control.DeepSeq (force)

runWithTimeout :: String -> IO (Maybe String)
runWithTimeout s = do
    mvar <- newEmptyMVar
    threadId <- forkIO  $ do
        res <- interp s
        newRes <- evaluate $ force res
        putMVar mvar newRes

    threadKiller <- forkIO $ do
        threadDelay $ 2 * (10^6) -- 2 seconds
        killThread threadId
        putMVar mvar Nothing

    res <- takeMVar mvar
    killThread threadKiller
    return res

errToMaybe :: Show e => Either e String -> Maybe String
errToMaybe (Left e) = Just $ "error " <> (unwords . lines) (show e)
errToMaybe (Right a) = Just a

runHString :: String -> IO String
runHString s = do
    res <- runWithTimeout s
    return $ maybe "unknown errror" (take 200) (res <|> Just "Timed out")

interp :: String -> IO (Maybe String)
interp s = fmap errToMaybe $ runInterpreter $ do
    setImports ["Prelude", "Control.Monad"]
    interpret ("show $ " <> s) (as :: String)

