module Twitch.Commands.Runh (
    runHString
) where

import Control.Monad (join)
import System.Directory.Internal.Prelude (timeout, fromMaybe)
import Language.Haskell.Interpreter (as, runInterpreter, setImports, interpret)

errToMaybe :: Show e => Either e String -> Maybe String
errToMaybe (Left e) = Just $ "error " <> (unwords . lines) (show e)
errToMaybe (Right a) = Just a

runWithTimeout :: String -> IO (Maybe String)
runWithTimeout s = fmap join . timeout (2 * (10 ^ 6)) $ interp s

runHString :: String -> IO String
runHString s = do
    res <- runWithTimeout s
    return $ maybe "unknown errror" (take 200) (res <* Just "Timed out")


interp :: String -> IO (Maybe String)
interp s = fmap errToMaybe $ runInterpreter $ do
    setImports ["Prelude", "Control.Monad"]
    interpret ("show $ " <> s) (as :: String)

