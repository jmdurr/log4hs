module Logging.Log4hs (defLoggerConfig,defLoggerCtx, withLogging, logMsg, module Logging.Log4hs.Types) where
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


defLoggerConfig = LogConfig { configLevel = WARN
                            , configAppenders = ["console"]
                            , configChildren = []
                            , configName = ""}

defLoggerCtx :: MonadIO m => LogContext m
defLoggerCtx = LogContext { ctxAppenders = [consoleAppender "console" SYSTEM_OUT (patternLayout (pack "%maxLen{%d{%T%Q}}{13} [%T] %level %logger{36} - %msg%n"))]
                          , ctxConfigs = defLoggerConfig
                          , ctxTime = liftIO getCurrentTime
                          , ctxProcessId = liftIO getProcessId
                          , ctxThreadId = liftIO myThreadId
                          }


withLogging :: Monad m => LogContext m -> Logger m a -> m a
withLogging ctx f = runReaderT f ctx

getAppenders :: Monad m => LogContext m -> LogConfig -> [LogAppender m]
getAppenders ctx cfg = mapMaybe (`lookup` map (\(n,a,_) -> (n,a)) (ctxAppenders ctx)) (configAppenders cfg)

getConfig :: Monad m => [String] -> Logger m LogConfig
getConfig [] = reader ctxConfigs
getConfig nm = do
    ctx <- ask
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


logMsg :: Monad m => [String] -> LogLevel -> Text -> [(Text,Text)] -> Logger m ()
logMsg nm lvl msg args = do
    c <- getConfig nm
    a <- asks (`getAppenders` c)
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
    where dolog = mapM_ (\a -> a nm lvl msg args)


