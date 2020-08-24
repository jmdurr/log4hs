module Logging.Log4hs (defLoggerConfig,defLoggerCtx, mkLogState, logMsg, logShow, logIO, module Logging.Log4hs.Types) where
import           Control.Applicative             ((<|>))
import           Control.Concurrent              (myThreadId)
import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Reader
import           Data.Foldable                   (asum)
import           Data.Maybe                      (mapMaybe)
import           Data.Text                       (Text, pack)
import           Data.Time.Clock                 (getCurrentTime)
import           Logging.Log4hs.Appender.Console
import           Logging.Log4hs.Layout.Pattern
import           Logging.Log4hs.Types
import           System.Process                  (getProcessId)

defLoggerConfig :: LogConfig
defLoggerConfig = LogConfig { configLevel = WARN
                            , configAppenders = ["console"]
                            , configChildren = []
                            , configName = ""}

defLoggerCtx :: (MonadIO m, LogVarProvider m) => LogContext m
defLoggerCtx = LogContext { ctxAppenders = [("console",consoleAppender SYSTEM_OUT (patternLayout (pack "%maxLen{%d{%T%Q}}{13} [%T] %level %logger{36} - %msg%n")))]
                          , ctxConfigs = defLoggerConfig
                          , ctxTime = liftIO getCurrentTime
                          , ctxProcessId = liftIO getProcessId
                          , ctxThreadId = liftIO myThreadId
                          }


mkLogState :: Monad n => LogContext n -> n (LogState n)
mkLogState ctx = do
  apps <- mapM (\(n,am) -> am >>= \a -> return (n,a)) $ ctxAppenders ctx
  return LogState{context=ctx,appenders=apps}


getAppenders :: [(String,(LogAppender n, LogAppenderFinish n))] -> LogConfig -> [LogAppender n]
getAppenders apps cfg = mapMaybe (`lookup` map (\(n,(a,_)) -> (n,a)) apps) (configAppenders cfg)

getConfig :: (Monad m, HasLog m l) => [String] -> m LogConfig
getConfig [] = ctxConfigs . context <$> logState
getConfig nm = do
    ctx <- context <$> logState
    case asum (map (findConfig nm) (configChildren $ ctxConfigs ctx)) of
        Nothing -> return $ ctxConfigs ctx
        Just c  -> return c
    where findConfig [] _ = Nothing
          findConfig [nm'] cfg
            | configName cfg == nm' = Just cfg
            | otherwise =  Nothing
          findConfig (nm':nms) cfg
            | configName cfg == nm' = asum (map (findConfig nms) (configChildren cfg)) <|> Just cfg
            | otherwise = Nothing


logMsg :: (HasLog m l) => [String] -> LogLevel -> Text -> [(Text,Text)] -> m ()
logMsg nm lvl msg args = do
    c <- getConfig nm
    a <- flip getAppenders c . appenders <$> logState
    case (configLevel c, lvl) of
        (ERROR,ERROR) -> dolog a
        (WARN,ERROR)  -> dolog a
        (WARN,WARN)   -> dolog a
        (INFO,ERROR)  -> dolog a
        (INFO,WARN)   -> dolog a
        (INFO,INFO)   -> dolog a
        (DEBUG,ERROR) -> dolog a
        (DEBUG,WARN)  -> dolog a
        (DEBUG,INFO)  -> dolog a
        (DEBUG,DEBUG) -> dolog a
        (TRACE,_)     -> dolog a
        (_,_)         -> return ()
    where dolog app = subLog $ mapM_ (\a -> a nm lvl msg args) app

logShow :: (HasLog m l, Show s) => [String] -> LogLevel -> s -> m ()
logShow nm l s = logMsg nm l (pack (show s)) []

logIO :: LogState IO -> LoggerT IO IO () -> IO ()
logIO st act = runReaderT (runLoggerT act) st

