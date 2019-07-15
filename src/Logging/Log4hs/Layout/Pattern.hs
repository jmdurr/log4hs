module Logging.Log4hs.Layout.Pattern (patternLayout) where
import           Control.Concurrent             (ThreadId)
import           Control.Monad                  (void)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Reader     (asks)
import           Control.Monad.Trans.State.Lazy
import           Data.Attoparsec.Text
import           Data.Char                      (isControl)
import           Data.List                      (intercalate)
import           Data.Maybe                     (fromMaybe, isJust)
import qualified Data.Text                      as T
import           Data.Text.Lazy                 (toStrict)
import qualified Data.Text.Lazy.Builder         as TB
import           Data.Time.Clock                (UTCTime)
import           Data.Time.Format               (defaultTimeLocale, formatTime)
import           Logging.Log4hs.Types
import           Text.Printf                    (printf)

data EncodeType = JSON | CRLF

data HighlightColor = BLACK | BLUE | RED | GREEN | YELLOW | MAGENTA | CYAN | WHITE deriving (Show,Read,Eq)

data PatternFlag = LoggerName Int
                 | Date String
                 | Encode EncodeType Pattern
                 | Highlight Pattern [(LogLevel,HighlightColor)]
                 | MaxLength Pattern Int
                 | Newline
                 | Pid
                 | Level [(LogLevel,String)]
                 | ThreadId
                 | Percent
                 | Message Bool
                 | KeyLookup String

data PatternSegment = PatternSegmentText T.Text | PatternSegmentFlag PatternFlag
newtype Pattern = Pattern [PatternSegment]

data PatternData m = PatternData { msg           :: T.Text
                                 , msgArgs       :: [(T.Text,T.Text)]
                                 , msgTimer      :: m UTCTime
                                 , msgLevel      :: LogLevel
                                 , msgLoggerName :: [String]
                                 , msgProcessId  :: m Int
                                 , msgThreadId   :: m ThreadId
}

maxPatternOut :: Int
maxPatternOut = 100 * 1024 * 1024

matchBraces :: Int -> Parser T.Text
matchBraces i = do
    t <- takeTill (== '}')
    let l = T.length (T.filter (== '{') t) in
        if i + l > 0
            then do
                char '}'
                r <- matchBraces (i + l - 1)
                return $ t <> T.singleton '}' <> r
            else
                return t

between :: Char -> Char -> Parser a -> Parser a
between s e p = do
    char s
    t <- matchBraces 0
    case parseOnly p t of
        Left e -> fail e
        Right t' -> do
            char e
            return t'

opt :: Parser a -> Parser (Maybe a)
opt p = option Nothing $ Just <$> requiredOpt p

requiredOpt :: Parser a -> Parser a
requiredOpt = between '{' '}'

loggerNameOpt :: Parser PatternFlag
loggerNameOpt = do
    choice [void $ char 'c', void $ string (T.pack "logger")]
    o <- opt $ signed decimal
    return $ LoggerName (fromMaybe maxPatternOut o)


date :: Parser PatternFlag
date = do
    choice [void $ string (T.pack "date"), void $ char 'd']
    s <- opt (many1 $ notChar '}')
    case s of
        Nothing -> fail "could not parse date"
        Just s' -> return (Date s')

encodeType :: Parser EncodeType
encodeType = choice [string (T.pack "CRLF") >> return CRLF, string (T.pack "JSON") >> return JSON]

encode :: Parser PatternFlag
encode = do
    choice [string (T.pack "encode"), string (T.pack "enc")]
    Encode . fromMaybe CRLF <$> opt encodeType <*> requiredOpt patternP

color :: Parser HighlightColor
color =
    choice [WHITE <$ asciiCI (T.pack "white")
           ,BLACK <$ asciiCI (T.pack "black")
           ,RED <$ asciiCI (T.pack "red")
           ,BLUE <$ asciiCI (T.pack "blue")
           ,MAGENTA <$ asciiCI (T.pack "magenta")
           ,GREEN <$ asciiCI (T.pack "green")
           ,YELLOW <$ asciiCI (T.pack "yellow")
           ,CYAN <$ asciiCI (T.pack "cyan")
           ]

logLevel :: Parser LogLevel
logLevel = choice [ERROR <$ string (T.pack "ERROR")
               ,WARN <$ string (T.pack "WARN")
               ,INFO <$ string (T.pack "INFO")
               ,DEBUG <$ string (T.pack "DEBUG")
               ,TRACE <$ string (T.pack "TRACE")
               ]
spaces :: Parser ()
spaces = void $ many' space

nvPair :: Parser n -> Parser v -> Parser [(n,v)]
nvPair pn pv = do
    spaces
    n <- pn
    spaces
    char '='
    spaces
    v <- pv
    spaces
    rs <- option Nothing (char ',' >> (Just <$> nvPair pn pv))
    return $ case rs of
        Nothing  -> [(n,v)]
        Just rs' -> (n,v):rs'

highlightStyle :: Parser [(LogLevel,HighlightColor)]
highlightStyle = nvPair logLevel color

highlight :: Parser PatternFlag
highlight = do
    string (T.pack "highlight")
    p <- requiredOpt patternP
    s <- requiredOpt highlightStyle
    return $ Highlight p s

maxLength :: Parser PatternFlag
maxLength = do
    choice [string (T.pack "maxLength"), string (T.pack "maxLen")]
    p <- requiredOpt patternP
    i <- requiredOpt decimal
    return $ MaxLength p i

newline :: Parser PatternFlag
newline = Newline <$ char 'n'

pid :: Parser PatternFlag
pid = Pid <$ choice [string (T.pack "pid"), string (T.pack "processId")]

levelAliases :: Parser [(LogLevel,String)]
levelAliases = nvPair logLevel (T.unpack <$> takeWhile1 (\c -> c /= ' ' && c /= ',' && c /= '}'))

level :: Parser PatternFlag
level = do
    choice [void $ char 'p', void $ string (T.pack "level")]
    p <- opt levelAliases
    return $ Level $ fromMaybe [] p

threadId :: Parser PatternFlag
threadId = ThreadId <$ choice [void $ char 'T', void $ string (T.pack "threadId"), void $ string (T.pack "tid")]

percent :: Parser PatternFlag
percent = Percent <$ char '%'

message :: Parser PatternFlag
message = do
    choice [string (T.pack "message"), string (T.pack "msg"), string (T.pack "m")]
    Message . isJust <$> opt (string (T.pack "nolookups"))

keylookup :: Parser PatternFlag
keylookup = do
    choice [void $ char 'K', void $ string (T.pack "map"), void $ string (T.pack "MAP")]
    k <- requiredOpt $ many1 anyChar
    return $ KeyLookup k

pctFlag :: Parser PatternFlag
pctFlag = char '%' >> choice [highlight,percent,threadId,pid,maxLength,message,encode,level,date,loggerNameOpt,newline, keylookup]

patternP :: Parser Pattern -- named patternP to avoid hlint pattern parse error
patternP = do
    p <- many' $ choice [PatternSegmentFlag <$> pctFlag, PatternSegmentText . T.pack <$> many1 (notChar '%')]
    endOfInput
    return $ Pattern p


expandPattern :: Monad m => [PatternSegment] -> Int -> PatternData m -> m TB.Builder
expandPattern [] _ _ = return $ TB.fromString ""
expandPattern _ 0 _  = return $ TB.fromString ""
expandPattern (PatternSegmentFlag (LoggerName i) : ps) len pdata = mappend (TB.fromString t) <$> expandPattern ps (len - length t) pdata
    where
        dr | i < 0 = Prelude.drop (abs i)
           | i > length (msgLoggerName pdata) = id
           | otherwise = Prelude.drop (length (msgLoggerName pdata) - i)
        t = Prelude.take len $ intercalate "." $ dr (msgLoggerName pdata)

expandPattern (PatternSegmentFlag (Date s) : ps) len pdata = do
        tm <- msgTimer pdata
        mappend (TB.fromString (t tm)) <$> expandPattern ps (len - length (t tm)) pdata
    where t tm = Prelude.take len (formatTime defaultTimeLocale s tm)


expandPattern (PatternSegmentFlag (Encode CRLF (Pattern ps')): ps) len pdata = do
        t <- expandPattern ps' len pdata
        (t',len') <- trunc t
        mappend t' <$> expandPattern ps len' pdata
    where crlf tb l c = case (c, l > 1)  of
                ('\r',True) -> (tb <> TB.singleton '\\' <> TB.singleton 'r',l-2)
                ('\n',True) -> (tb <> TB.singleton '\\' <> TB.singleton 'n', l - 2)
                (_,True) -> (tb <> TB.singleton c, l - 1)
                ('\r',False) -> (tb,0)
                ('\n',False) -> (tb,0)
                (_,False) -> if l > 0 then (tb <> TB.singleton c,l-1) else (tb,l)
          trunc t = return $ T.foldl (\(b,l) c -> crlf b l c) (TB.fromString "", len) (toStrict $ TB.toLazyText t)

expandPattern (PatternSegmentFlag (Encode JSON (Pattern ps')) : ps) len pdata = do
        t <- expandPattern ps' len pdata
        (t',len') <- return $ T.foldl (\(b,l) c -> loweruni b l c) (TB.fromString "", len) (toStrict $ TB.toLazyText t)
        mappend t' <$> expandPattern ps len' pdata
    where loweruni tb l c
            | (c >= '\x0000' && c <= '\x001F') || isControl c = if l > 5 then (tb <> TB.fromString (printf "\\u%-04.4u" c), l - 6) else (tb,0)
            | c == '"' = if l > 1 then (tb <> TB.fromString "\\\"", l - 2) else (tb,0)
            | c == '\\' = if l > 1 then (tb <> TB.fromString "\\\'", l - 2) else (tb,0)
            | l > 0 = (tb <> TB.singleton c, l - 1)
            | otherwise = (tb,0)

expandPattern (PatternSegmentFlag (Highlight (Pattern ps') s) : ps) len pdata = do
        t <- expandPattern ps' len pdata
        case (lookup (msgLevel pdata) s, len - 8 > tlen t) of
            (_,False)    -> mappend t <$> expandPattern ps (len - tlen t) pdata
            (Nothing,_)      -> mappend t <$> expandPattern ps (len - tlen t) pdata
            (Just WHITE,True)   -> colorize "37" t
            (Just BLACK,True)   -> colorize "30" t
            (Just RED,True)     -> colorize "31" t
            (Just GREEN,True)   -> colorize "32" t
            (Just YELLOW,True)  -> colorize "33" t
            (Just BLUE,True)    -> colorize "34" t
            (Just MAGENTA,True) -> colorize "35" t
            (Just CYAN,True)    -> colorize "36" t
    where colorize n t = mappend (TB.fromString "\x001b[" <>  TB.fromString n <> t <> TB.fromString "\x001b[0m") <$> expandPattern ps (len - tlen t - 8) pdata
          tlen t = T.length . toStrict $ TB.toLazyText t

expandPattern (PatternSegmentFlag (MaxLength (Pattern p) l) : ps) len pdata = do
    t <- expandPattern p l pdata
    mappend t <$> expandPattern ps (len - T.length (toStrict (TB.toLazyText t))) pdata

expandPattern (PatternSegmentFlag Newline : ps) len pdata
    | len > 0 = mappend (TB.singleton '\n') <$> expandPattern ps (len - 1) pdata
    | otherwise = return $ TB.fromString ""

expandPattern (PatternSegmentFlag Pid : ps) len pdata = do
    pd <- pid
    mappend (TB.fromText (T.take len pd)) <$> expandPattern ps (len - T.length pd) pdata
    where pid = T.pack . show <$> msgProcessId pdata

expandPattern (PatternSegmentFlag (Level lvls) : ps) len pdata = mappend (TB.fromString lvlstr) <$> expandPattern ps (len - length lvlstr) pdata
    where lvlstr = Prelude.take len $ fromMaybe (show (msgLevel pdata)) $ lookup (msgLevel pdata) lvls

expandPattern (PatternSegmentFlag ThreadId : ps) len pdata = do
    t <- T.take len . T.pack . show <$> msgThreadId pdata
    mappend (TB.fromText t) <$> expandPattern ps (len - T.length t) pdata

expandPattern (PatternSegmentFlag Percent : ps) len pdata
    | len > 0 = mappend (TB.singleton '%') <$> expandPattern ps (len - 1) pdata
    | otherwise = return $ TB.fromString ""


expandPattern (PatternSegmentFlag (Message b) : ps) len pdata =
    mappend (TB.fromText msgT) <$> expandPattern ps (len - T.length msgT) pdata
    where msgT = T.take len (msg pdata)

expandPattern (PatternSegmentText t : ps) len pdata =
    mappend (TB.fromText msgT) <$> expandPattern ps (len - T.length msgT) pdata
    where msgT = T.take len t

expandPattern (PatternSegmentFlag (KeyLookup k) : ps) len pdata =
    mappend (TB.fromText kv) <$> expandPattern ps (len - T.length kv) pdata
    where kv = T.take len $ fromMaybe (T.pack "NoKey") $ lookup (T.pack k) (msgArgs pdata)

patternLayout ::  Monad m => T.Text -> Layout m
patternLayout ptrn =
    case parseOnly patternP ptrn of
        Left e -> \nm lvl msg' args -> return $ T.pack "pattern layout parse failure: " <> T.pack e
        Right (Pattern ps) -> \nm lvl msg' args ->
                toStrict . TB.toLazyText <$> do
                    lt <- asks ctxTime
                    lp <- asks ctxProcessId
                    ltid <- asks ctxThreadId
                    lift $ expandPattern ps maxPatternOut PatternData{msgLoggerName=nm,msgLevel=lvl,msg=msg',msgArgs=args,msgTimer=lt,msgProcessId=lp,msgThreadId=ltid}

