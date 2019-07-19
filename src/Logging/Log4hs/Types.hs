{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Logging.Log4hs.Types where
import           Control.Applicative       ((<|>))
import           Control.Concurrent        (ThreadId, myThreadId)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Foldable             (asum)
import           Data.Maybe                (mapMaybe)
import           Data.Text                 (Text, pack, unpack)
import           Data.Time.Clock           (UTCTime, getCurrentTime)
import           System.IO                 (hPutStr, stderr, stdout)
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

-- logger with underlying monad m where logging will actually happen ex: IO
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
class (MonadTrans m, LogVarProvider n, Monad (m n)) => HasLog m n where
    logState :: (m n) (LogState n)
    withLogState :: (LogState n -> a) -> (m n) a
    withLogState f = f <$> logState
    withLog :: LogState n -> (m n) a -> n a

instance LogVarProvider m => HasLog LoggerT m where
    logState = LoggerT { runLoggerT = ask}
    withLog s la = runReaderT (runLoggerT la) s


instance LogVarProvider IO where
    provideTime = getCurrentTime
    provideProcessId = getProcessId
    provideThreadId = myThreadId
