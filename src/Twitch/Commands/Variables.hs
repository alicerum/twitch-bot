{-# LANGUAGE OverloadedStrings #-}

module Twitch.Commands.Variables
    ( letCommand
    , unletCommand
    , clearCommand
) where

import Control.Lens
import Data.Text
import Data.List.Lens
import Twitch.Types
import Text.Parsec
import Text.Parsec.Text
import Control.Monad.Trans.State
import Control.Monad.Except
import Twitch.Commands.Runh (runInterp)
import Language.Haskell.Interpreter (InterpreterError)

bindSymbol :: Parser Char
bindSymbol = letter <|> digit <|> char '\'' <|> char '_'
                <|> char '-'

bindName :: Parser Text
bindName = pack <$> ((:) <$> letter <*> many bindSymbol)

delim :: Parser Char
delim = spaces *> char '=' <* spaces

expr :: Parser Text
expr = pack <$> manyTill anyChar eof

parseCommand :: Parser (Text, Text)
parseCommand = (,) <$> bindName <* delim <*> expr

parseAndTry :: VarList -> Text -> ExceptT String IO (VarName, VarDef)
parseAndTry varList command = do
    (varName, varDef) <- withExceptT show
                            $ ExceptT
                            $ return
                            $ parse parseCommand "" command
    void $ withExceptT show
            $ runInterp varList (unpack varDef)
    return (varName, varDef)

getVarList :: CommandWithState VarList
getVarList = do
    state <- get
    return $ state ^. variables

addToVarList :: (VarName, VarDef) -> CommandWithState ()
addToVarList p = do
    state <- get
    put $ state & variables <>~ [p]

letCommand :: Command Text
letCommand user command = do
    vl <- getVarList
    eitherV <- liftIO $ runExceptT $ parseAndTry vl command
    case eitherV of
                Left e -> return $ "Error while processing variable: " <> pack e
                Right v -> do
                    addToVarList v
                    return $ "variable " <> fst v <> " added"

unletCommand :: Command Text
unletCommand user command = do
    return "fff"

clearCommand :: Command Text
clearCommand user command = do
    s <- get
    -- emptying out the variables
    put $ s & variables .~ []
    return "Variables cleared"

