{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE MultiParamTypeClasses  #-}

module Logging.Log4hs.Types where
import           Control.Concurrent        (ThreadId, myThreadId)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Text                 (Text)
import           Data.Time.Clock           (UTCTime, getCurrentTime)
import           System.Process            (getProcessId)


data LogLevel = ERROR | WARN | INFO | DEBUG | TRACE deriving (Show,Eq,Read)

type Layout m = [String] -> LogLevel -> Text -> [(Text,Text)] -> m Text

type LogAppender m = [String] -> LogLevel -> Text -> [(Text,Text)] -> m ()

type LogAppenderFinish m = m ()


data LogContext m = LogContext { ctxAppenders :: [(String, m (LogAppender m,LogAppenderFinish m))]
                               , ctxConfigs   :: LogConfig
                               , ctxTime      :: m UTCTime
                               , ctxProcessId :: m Int
                               , ctxThreadId  :: m ThreadId
                               }

data LogState m = LogState {context :: LogContext m, appenders :: [(String,(LogAppender m, LogAppenderFinish m))]}

--data ReliabilityStrategy = ReliabilityStrategy {}
data LogConfig = LogConfig { configLevel     :: LogLevel
                           , configAppenders :: [String]
                           --, configReliabilityStrategy :: ReliabilityStrategy
                           , configChildren  :: [LogConfig]
                           , configName      :: String
                           }

-- logger state with underlying monad m where logging will actually happen ex: IO
newtype LoggerT m a = LoggerT { runLoggerT :: ReaderT (LogState m) m a }

instance Monad m => Functor (LoggerT m) where
    fmap ab fa = LoggerT { runLoggerT = ab <$> runLoggerT fa}

instance Monad m => Applicative (LoggerT m) where
    pure a = LoggerT { runLoggerT = pure a}
    (<*>) fab fa = LoggerT { runLoggerT = runLoggerT fa >>= \a -> runLoggerT fab >>= \ab -> return $ ab a}

instance Monad m => Monad (LoggerT m) where
    return = pure
    (>>=) ma amb = LoggerT { runLoggerT = runLoggerT ma >>= \a -> runLoggerT (amb a)}

instance MonadIO m => MonadIO (LoggerT m) where
    liftIO ioa = LoggerT { runLoggerT = lift . liftIO $ ioa}

instance MonadTrans LoggerT where
    lift ma = LoggerT { runLoggerT = lift ma}

class Monad m => LogVarProvider m where
    provideTime :: m UTCTime
    provideProcessId :: m Int
    provideThreadId :: m ThreadId

-- HasLog where m is the monad with the logging state etc and n is the monad where underlying logging happens
class (Monad m, LogVarProvider n) => HasLog m n | m -> n where
    logState :: m (LogState n)
    runInLogging :: n a -> m a
    withLogState :: (LogState n -> a) -> m a
    withLogState f = f <$> logState

instance (LogVarProvider m) => HasLog (LoggerT m) m where
    logState = LoggerT { runLoggerT = ask}
    runInLogging = lift


instance LogVarProvider IO where
    provideTime = getCurrentTime
    provideProcessId = getProcessId
    provideThreadId = myThreadId
