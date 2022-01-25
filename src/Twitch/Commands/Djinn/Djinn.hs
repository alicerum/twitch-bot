{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--
module Twitch.Commands.Djinn.Djinn(eval, State(..), startState) where
import Data.Char(isAlpha, isSpace)
import Data.List(sortBy, nub, intersperse)
import Data.Ratio
import Text.ParserCombinators.ReadP
import Control.Monad(when)
import Control.Monad.Error()
import System.Exit
import Control.Monad.Trans.Class
import System.Environment

import Twitch.Commands.Djinn.REPL
import Twitch.Commands.Djinn.LJT
import Twitch.Commands.Djinn.HTypes
import Twitch.Commands.Djinn.HCheck(htCheckEnv, htCheckType)
import Twitch.Commands.Djinn.Help
import Control.Monad.Trans.Writer (runWriter, tell, WriterT (runWriterT))

--import Debug.Trace

version :: String
version = "version 2011-07-23"


usage :: IO ()
usage = putStrLn "Usage: djinn [option ...] [file ...]"


data State = State {
    synonyms :: [(HSymbol, ([HSymbol], HType, HKind))],
    axioms :: [(HSymbol, HType)],
    classes :: [ClassDef],
    multi :: Bool,
    sorted :: Bool,
    debug :: Bool,
    cutOff :: Int
    }
    deriving (Show)

startState :: State
startState = State {
    synonyms = syns,
    classes = clss,
    axioms = [],
    multi = False,
    sorted = True,
    debug = False,
    cutOff = 200
    }
 where syns = either (const $ error "Bad initial environment") id $ htCheckEnv $ reverse [
        ("()",     ([],        HTUnion [("()",[])],                         undefined)),
        ("Either", (["a","b"], HTUnion [("Left", [a]), ("Right", [b])],     undefined)),
        ("Maybe",  (["a"],     HTUnion [("Nothing", []), ("Just", [a])],    undefined)),
        ("Bool",   ([],        HTUnion [("False", []), ("True", [])],       undefined)),
        ("Void",   ([],        HTUnion [],                                  undefined)),
        ("Not",    (["x"],     htNot "x",                                   undefined))
        ]
       clss = [("Eq", (["a"], [("==", a `HTArrow` (a `HTArrow` HTCon "Bool"))])),
               ("Monad", (["m"], [("return", a `HTArrow` ma),
                                  (">>=", ma `HTArrow` ((a `HTArrow` mb) `HTArrow` mb))]))
              ] where ma = HTApp m a; mb = HTApp m b
       a = HTVar "a"
       b = HTVar "b"
       m = HTVar "m"


inIt :: State -> IO (String, State)
inIt state = do
    putStrLn $ "Welcome to Djinn " ++ version ++ "."
    putStrLn $ "Type :h to get help."
    return ("Djinn> ", state)

eval :: State -> String -> IO (String, State)
eval s line =
    case filter (null . snd) (readP_to_S pCmd line) of
    [] -> do
                return ("Cannot parse command", s)
    (cmd, "") : _ -> runCmd s cmd
    _ -> error "eval"

exit :: State -> IO ()
exit _s = do
    putStrLn "Bye."
    return ()

type Context = (HSymbol, [HType])
type ClassDef = (HSymbol, ([HSymbol], [Method]))

data Cmd = Help Bool | Quit | Add HSymbol HType | Query HSymbol [Context] HType | Del HSymbol |  Noop | Env |
           Type (HSymbol, ([HSymbol], HType, HKind)) | Set (State -> State) | Clear | Class ClassDef |
           QueryInstance [Context] HSymbol [HType]

pCmd :: ReadP Cmd
pCmd = do
    skipSpaces
    let adds (':':s) p = do schar ':'; pPrefix (takeWhile (/= ' ') s); c <- p; skipSpaces; return c
        adds _ p = do c <- p; skipSpaces; return c
    cmd <- foldr1 (+++) [ adds s p | (s, _, p) <- commands ]
    skipSpaces
    return cmd

pPrefix :: String -> ReadP String
pPrefix s = do
    skipSpaces
    cs <- look
    let w = takeWhile isAlpha cs
    if isPrefix w s then
        string w
     else
        pfail

isPrefix :: String -> String -> Bool
isPrefix p s = not (null p) && length p <= length s && take (length p) s == p

runCmd :: State -> Cmd -> IO (String, State)
runCmd s Noop = return ("", s)
runCmd s (Help verbose) = do
    let help = helpText ++ unlines (map getHelp commands) ++ getSettings s
    if verbose then return (help <> "\n" <> verboseHelp, s)
    else return (help, s)
runCmd s Quit = 
    return ("", s)
runCmd s (Add i t) = 
    case htCheckType (synonyms s) t of
    Left msg -> return ("Error: " ++ msg, s)
    Right _ -> return ("done", s { axioms = (i, t) : filter ((/= i) . fst) (axioms s) })
runCmd _ Clear =
    return ("state cleared", startState)
runCmd s (Del i) = 
    return ("deleted", s { axioms   = filter ((i /=) . fst) (axioms s)
                     , synonyms = filter ((i /=) . fst) (synonyms s)
                     , classes = filter ((i /=) . fst) (classes s) })
runCmd s Env = do
--    print s
    let tname t = if isHTUnion t then "data" else "type"
        showd (HTUnion []) = ""
        showd t = " = " ++ show t

    let res = map (\ (i, (vs, t, _)) -> tname t ++ " " ++ unwords (i:vs) ++ showd t) (reverse $ synonyms s)
              <> map (\ (i, t) -> prHSymbolOp i ++ " :: " ++ show t) (reverse $ axioms s)
              <> map showClass (reverse $ classes s)
    return (unlines res, s)
runCmd s (Type syn) = do
    case htCheckEnv (syn : synonyms s) of
        Left msg -> do
            let msg = "Error: " ++ msg
            return (msg, s)
        Right syns -> return ("type added", s { synonyms = syns })
runCmd s (Set f) =
    return ("done", f s)
runCmd s (Query i ctx g) =
    query True s i ctx g
runCmd s (Class c) = do
    return ("Class added", s { classes = c : classes s })
runCmd s (QueryInstance ctx cls ts) =
    case lookup cls (classes s) of
    Nothing -> return ("No class " ++ cls, s)
    Just (vs, ms) -> if length ts /= length vs then return ("Wrong number of type arguments", s) else do
               let sctx = if null ctx then "" else showContexts ctx ++ " => "
               let r = zip vs ts
                   method (i, t) = runWriterT $ do
                       tell ["   "]
                       (msg, s) <- lift $ query False s i ctx (substHT r t)
                       tell [msg]
                       return s
               putStrLn $ "instance " ++ sctx ++ show (foldl HTApp (HTCon cls) ts) ++ " where"
               rr <- mapM method ms
               let res = ("instance " ++ sctx ++ show (foldl HTApp (HTCon cls) ts) ++ " where") : map (mconcat . snd) rr
               return (unlines res, s)

query :: Bool -> State -> String -> [Context] -> HType -> IO (String, State)
query prType s i ctx g =
   case htCheckType (synonyms s) g >> mapM (ctxLookup (classes s)) ctx of
   Left msg -> return ("Error: " <> msg, s)
   Right mss -> (\(s, msg) -> return (unlines msg, s)) $ runWriter $ do
    let form = hTypeToFormula (synonyms s) g
        env = [ (Symbol v, hTypeToFormula (synonyms s) t) | (v, t) <- axioms s ] ++ ctxEnv
        ctxEnv = [ (Symbol v, hTypeToFormula (synonyms s) t) | ms <- mss, (v, t) <- ms ]
        mpr = prove (multi s || sorted s) env form
    when (debug s) $ tell ["*** " ++ show form]
    case mpr of
        [] -> do
            tell ["-- " ++ i ++ " cannot be realized."]
            return s
        ps -> do
	    let ps' = take (cutOff s) ps
            let score p =
                   let c = termToHClause i p
                       bvs = getBinderVars c
                       r = if null bvs then (0, 0) else (length (filter (== "_") bvs) % length bvs, length bvs)
                   in  --trace (hPrClause c ++ " ++++ " ++ show r)
                       (r, c)
                e:es = nub $ 
                        if sorted s then
                            map snd $ sortBy (\ (x,_) (y,_) -> compare x y) $ map score ps'
                        else
                            map (termToHClause i) ps'
                pr = (\s -> tell [s]) . hPrClause
                sctx = if null ctx then "" else showContexts ctx ++ " => "
            when (debug s) $ tell ["+++ " ++ show (head ps)]
            when prType $ tell [prHSymbolOp i ++ " :: " ++ sctx ++ show g]
            pr e
            when (multi s) $ mapM_ (\ x -> tell ["-- or"] >> pr x) es
            return s


stripComments :: String -> String
stripComments "" = ""
stripComments ('-':'-':cs) = skip cs
  where skip "" = ""
        skip s@('\n':_) = stripComments s
        skip (_:s) = skip s
stripComments (c:cs) = c : stripComments cs

showClass :: ClassDef -> String
showClass (c, (as, ms)) = "class " ++ showContext (c, map HTVar as) ++ " where " ++ concat (intersperse "; " $ map sm ms)
  where sm (i, t) = prHSymbolOp i ++ " :: " ++ show t

showContext :: Context -> String
showContext (c, as) = show $ foldl HTApp (HTCon c) as

showContexts :: [Context] -> String
showContexts [] = ""
showContexts cs = "(" ++ concat (intersperse ", " $ map showContext cs) ++ ")"

ctxLookup :: [ClassDef] -> Context -> Either String [Method]
ctxLookup clss (c, as) =
    case lookup c clss of
    Nothing -> Left $ "Class not found: " ++ c
    Just (ps, ms) -> Right [(m, substHT (zip ps as) t) | (m, t) <- ms ]


commands :: [(String, String, ReadP Cmd)]
commands = [
        (":clear",              "Clear the envirnment",         return Clear),
        (":delete <sym>",       "Delete from environment.",     pDel),
        (":environment",        "Show environment",             return Env),
        (":help",               "Print this message.",          return (Help False)),
        (":quit",               "Quit program.",                return Quit),
        (":set <option>",       "Set options",                  pSet),
        (":verboseHelp",        "Print verbose help.",          return (Help True)),
        ("type <sym> <vars> = <type>", "Add a type synonym",    pType),
        ("data <sym> <vars> = <datatype>", "Add a data type",   pData),
        ("class <sym> <vars> where <methods>", "Add a class",   pClass),
        ("<sym> :: <type>",     "Add to environment",           pAdd),
        ("? <sym> :: <type>",   "Query",                        pQuery'),
        ("<sym> ? <type>",      "Query",                        pQuery),
        ("?instance <sym> <types>","Query instance",            pQueryInstance),
        ("",                    "",                             return Noop)
        ]

options :: [(String, String, State->Bool, Bool->State->State)]
options = [
          ("multi",             "print multiple solutions",     multi,  \ v s -> s { multi  = v }),
          ("sorted",            "sort solutions",               sorted, \ v s -> s { sorted = v }),
          ("debug",             "debug mode",                   debug,  \ v s -> s { debug  = v })
          ]

getHelp :: (String, String, a) -> String
getHelp (cmd, help, _) = cmd ++ replicate (35 - length cmd) ' ' ++ help

pDel :: ReadP Cmd
pDel = do
    s <- pHSymbol True +++ pHSymbolOp
    return $ Del s

pAdd :: ReadP Cmd
pAdd = do
    i <- pHSymbolOp
    sstring "::"
    t <- pHType
    optional $ schar ';'
    return $ Add i t

pQuery :: ReadP Cmd
pQuery = do
    i <- pHSymbolOp
    schar '?'
    c <- option [] pContext
    t <- pHType
    optional $ schar ';'
    return $ Query i c t

pQuery' :: ReadP Cmd
pQuery' = do
    schar '?'
    i <- pHSymbolOp
    sstring "::"
    c <- option [] pContext
    t <- pHType
    optional $ schar ';'
    return $ Query i c t

pQueryInstance :: ReadP Cmd
pQueryInstance = do
    schar '?'
    sstring "instance"
    c <- option [] pContext
    cls <- pHSymbol True
    ts <- many1 pHTAtom
    optional $ schar ';'
    return $ QueryInstance c cls ts

pContext :: ReadP [Context]
pContext = do
    let pCtx = do c <- pHSymbol True; ts <- many pHTAtom; return (c, ts)
    ctx <- 
        do
          schar '('
          ctx <- sepBy1 pCtx (schar ',')
          schar ')'
          return ctx
       +++
        do
          ctx <- pCtx
          return [ctx]
    sstring "=>"
    return ctx

pType :: ReadP Cmd
pType = do
    sstring "type"
    syn <- pHSymbol True
    do args <- many (pHSymbol False)
       schar '='
       t <- pHType
       return $ Type (syn, (args, t, undefined))
     +++
      do
       schar ':'; char ':'
       k <- pHKind
       return $ Type (syn, ([], HTAbstract syn k, undefined))

pData :: ReadP Cmd
pData = do
    sstring "data"
    syn <- pHSymbol True
    args <- many (pHSymbol False)
    do schar '='
       t <- pHDataType
       return $ Type (syn, (args, t, undefined))
      +++
     do
       return $ Type (syn, (args, HTUnion [], undefined))

pClass :: ReadP Cmd
pClass = do
    sstring "class"
    cls <- pHSymbol True
    args <- many (pHSymbol False)
    sstring "where"
    mets <- sepBy pMethod (schar ';')
    return $ Class (cls, (args, mets))

type Method = (HSymbol, HType)

pMethod :: ReadP Method
pMethod = do
    i <- pHSymbolOp
    sstring "::"
    t <- pHType
    return (i, t)

pHSymbolOp :: ReadP HSymbol
pHSymbolOp = do
    let pOpSym = satisfy (`elem` "~!#$%^&*-+=<>.:")
    pHSymbol False +++ do schar '('; op <- many1 pOpSym; schar ')'; return op

pSet :: ReadP Cmd
pSet = pSetFlag +++ pSetVal

pSetFlag :: ReadP Cmd
pSetFlag = do
    val <- (do schar '+'; return True) +++ (do schar '-'; return False) 
    f <- foldr (+++) pfail [ do pPrefix s; return (set val) | (s, _, _, set) <- options ]
    return $ Set $ f

pSetVal :: ReadP Cmd
pSetVal = do
    pPrefix "cutoff"
    schar '='
    n <- many1 (satisfy (`elem` ['0'..'9']))
    return $ Set $ \ s -> s { cutOff = read n }

schar :: Char -> ReadP ()
schar c = do
    skipSpaces
    char c
    return ()

sstring :: String -> ReadP ()
sstring s = do
    skipSpaces
    string s
    return ()

helpText :: String
helpText = "\
\Djinn is a program that generates Haskell code from a type.\n\
\Given a type the program will deduce an expression of this type,\n\
\if one exists.  If the Djinn says the type is not realizable it is\n\
\because there is no (total) expression of the given type.\n\
\Djinn only knows about tuples, ->, and some data types in the\n\
\initial environment (do :e for a list).\n\
\\n\
\Caveat emptor: The expression will have the right type, but it\n\
\may not be what you were looking for.\n\
\\n\
\Send any comments and feedback to lennart@augustsson.net\n\
\\n\
\Commands (may be abbreviated):\n\
\"

getSettings :: State -> String
getSettings s = unlines $ [
    "",
    "Current settings" ] ++ [ "    " ++ (if gett s then "+" else "-") ++ name ++ replicate (10 - length name) ' ' ++ descr |
                              (name, descr, gett, _set) <- options ] ++
    [ "    cutoff=" ++ show (cutOff s) ++ " maximum number of solutions generated" ]

