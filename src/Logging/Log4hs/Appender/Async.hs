{-# LANGUAGE FlexibleContexts #-}
module Logging.Log4hs.Appender.Async (asyncAppender) where
import           Control.Concurrent            (forkIO, killThread, myThreadId)
import           Control.Concurrent.STM        (atomically)
import           Control.Concurrent.STM.TBChan
import           Control.Exception.Base        (onException)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Trans.Reader
import           Data.Text                     (Text)
import           Data.Time.Clock               (getCurrentTime)
import           Logging.Log4hs
import           System.Process

childLogger :: [(String, (LogAppender IO,LogAppenderFinish IO))] -> TBChan ([String],LogLevel,Text,[(Text,Text)]) -> IO ()
childLogger chld c = untilKilled $ withLogging' ctx loop
    where loop = do
            (nm,lvl,msg,args) <- liftIO $ atomically $ readTBChan c
            logMsg nm lvl msg args
            loop
          ctx = LogContext {ctxAppenders = map (\(n,a) -> (n,return a)) chld
                           ,ctxConfigs = LogConfig {configLevel = TRACE
                                                   ,configAppenders=map fst chld
                                                   ,configChildren=[]
                                                   ,configName=""
                                                   }
                           ,ctxTime = getCurrentTime
                           ,ctxProcessId = getProcessId
                           ,ctxThreadId = myThreadId
                           }
          untilKilled m = onException m (mapM_ (\(_,(_,f)) -> f) chld)
          withLogging' :: LogContext IO -> LoggerT IO a -> IO a
          withLogging' ctx' la = do
            s <- mkLogState ctx'
            runReaderT (runLoggerT la) s

asyncAppender :: MonadIO m => [(String,m (LogAppender IO,LogAppenderFinish IO))] -> Int -> m (LogAppender m,LogAppenderFinish m)
asyncAppender chld qsz = do
    tc <- liftIO $ newTBChanIO qsz -- chan of (IO ())
    apps <- mapM (\(n,appm) -> appm >>= \appm' -> return (n,appm')) chld
    tid <- liftIO $ forkIO $ childLogger apps tc
    return (\m lvl msg args -> liftIO $ atomically $ writeTBChan tc (m,lvl,msg,args),liftIO $ killThread tid)
