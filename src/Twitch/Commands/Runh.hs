module Twitch.Commands.Runh (
    runHString
) where

import Data.List
import Data.Char
import Control.Monad (join, MonadPlus (mplus))
import Control.Concurrent
import Control.Applicative ((<|>))
import System.Directory.Internal.Prelude (timeout, fromMaybe)
import Language.Haskell.Interpreter (Extension (UnknownExtension), eval, interpret, runInterpreter, setImports, InterpreterError (WontCompile), errMsg, MonadTrans (lift))
import Control.Exception (catch, evaluate)
import qualified Control.Exception as E
import Control.DeepSeq
import Twitch.Types (CommandWithState)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT))

runWithTimeout :: String -> IO (Maybe String)
runWithTimeout s = do
    mvar <- newEmptyMVar
    threadId <- forkIO  $ do
        res <- interp s
        let resBegin = take 400 <$> res
        newRes <- evaluate $ resBegin `deepseq` resBegin
        putMVar mvar newRes

    threadKiller <- forkIO $ do
        threadDelay $ 2 * (10^6) -- 2 seconds
        killThread threadId
        putMVar mvar Nothing

    res <- takeMVar mvar
    killThread threadKiller
    return res

evaluateExpr :: String -> IO (Maybe String)
evaluateExpr s = do
    res <- interp s
    Just <$> (evaluate (force $ show s) `catch`
                    \(E.SomeException e) -> return ("Error: " <> show e))

errToMaybe :: Either InterpreterError String -> Maybe String
errToMaybe (Left e) = Just (printInterpErr e)
errToMaybe (Right a) = Just a

maybeErrToResult :: Maybe String -> String
maybeErrToResult = maybe "Timed Out" (take 400)

runHString :: String -> CommandWithState String
runHString s = lift . MaybeT $ Just . maybeErrToResult <$> runWithTimeout s

interp :: String -> IO (Maybe String)
interp s = fmap errToMaybe $ runInterpreter $ do
    setImports ["Prelude", "ShowFun", "Control.Monad", "Data.List", "Data.Char"
                , "Control.Lens", "Control.Applicative", "Control.Arrow"]
    -- interpret ("show $ " <> s) (as :: String)
    eval s
                 
printInterpErr :: InterpreterError -> String
printInterpErr (WontCompile errors) =
    -- if we get a compilation error we print it directly to avoid \"mueval: ...\"
    -- maybe it should go to stderr?
    concatMap (dropLinePosition . errMsg) errors
    where
      -- each error starts with the line position, which is uninteresting
      dropLinePosition e
          | Just s <- parseErr e =  s
          | otherwise = e -- if the parse fails we fallback on printing the whole error
      parseErr e = do s <- stripPrefix "<interactive>:" e
                      skipSpaces =<< (skipNumber =<< skipNumber s)
      skip x (y:xs) | x == y = Just xs
                    | otherwise = Nothing
      skip _ _ = Nothing
      skipNumber = skip ':' . dropWhile isDigit 
      skipSpaces xs = let xs' = dropWhile (==' ') xs
                      in skip '\n' xs' `mplus` return xs'
printInterpErr other = "ping my owner about this error: " <> show other
