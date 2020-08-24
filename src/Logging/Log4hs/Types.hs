{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE MultiParamTypeClasses  #-}

module Logging.Log4hs.Types where
import           Control.Concurrent        (ThreadId, myThreadId)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader
import Control.Monad.Reader.Class (ask)
import           Control.Monad.Trans.Class (lift)
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
newtype LoggerT n m a = LoggerT { runLoggerT :: ReaderT (LogState m) n a }

instance (Monad n, Monad m) => Functor (LoggerT n m) where
    fmap ab fa = LoggerT { runLoggerT = ab <$> runLoggerT fa}

instance (Monad n, Monad m) => Applicative (LoggerT n m) where
    pure a = LoggerT { runLoggerT = pure a}
    (<*>) fab fa = LoggerT { runLoggerT = runLoggerT fa >>= \a -> runLoggerT fab >>= \ab -> return $ ab a}

instance (Monad n, Monad m) => Monad (LoggerT n m) where
    return = pure
    (>>=) ma amb = LoggerT { runLoggerT = runLoggerT ma >>= \a -> runLoggerT (amb a)}

instance (MonadIO n, Monad m) => MonadIO (LoggerT n m) where
    liftIO ioa = LoggerT { runLoggerT = liftIO $ ioa}

class Monad m => LogVarProvider m where
    provideTime :: m UTCTime
    provideProcessId :: m Int
    provideThreadId :: m ThreadId
    
instance LogVarProvider IO where
    provideTime = getCurrentTime
    provideProcessId = getProcessId
    provideThreadId = myThreadId

instance LogVarProvider (LoggerT IO IO) where
    provideTime = LoggerT { runLoggerT = liftIO getCurrentTime }
    provideProcessId = LoggerT { runLoggerT = liftIO getProcessId}
    provideThreadId = LoggerT { runLoggerT = liftIO myThreadId}


class (Monad m, Monad n) => HasLog m n | m -> n where
    logState :: m (LogState n)
    subLog :: n a -> m a

instance HasLog (LoggerT IO IO) IO where
    logState = LoggerT ask
    subLog s = LoggerT { runLoggerT = lift s}


