module Logging.Log4hs.Appender.Async (asyncAppender) where
import           Control.Concurrent            (forkIO, killThread, myThreadId)
import           Control.Concurrent.STM        (atomically)
import           Control.Concurrent.STM.TBChan
import           Control.Exception.Base        (AsyncException (..),
                                                onException)
import           Control.Monad                 (forever)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Data.Text                     (Text)
import           Data.Time.Clock               (getCurrentTime)
import           Logging.Log4hs
import           System.Process


childLogger :: [(String,LogAppender IO,LogAppenderFinish IO)] -> TBChan ([String],LogLevel,Text,[(Text,Text)]) -> IO ()
childLogger chld c = untilKilled $ withLogging ctx $ forever $ do
        (nm,lvl,msg,args) <- liftIO $ atomically $ readTBChan c
        mapM_ (\(_,app,_) -> app nm lvl msg args) (ctxAppenders ctx)
    where ctx = LogContext {ctxAppenders = chld
                                              ,ctxConfigs = LogConfig {configLevel = TRACE
                                                                      ,configAppenders=map (\(n,_,_) -> n) chld
                                                                      ,configChildren=[]
                                                                      ,configName=""
                                                                      }
                                              ,ctxTime = getCurrentTime
                                              ,ctxProcessId = getProcessId
                                              ,ctxThreadId = myThreadId
                                              }
          untilKilled m = onException m (mapM_ (\(_,_,f) -> f) chld)

asyncAppender :: MonadIO m => String -> [(String,LogAppender IO,LogAppenderFinish IO)] -> Int -> m (String,LogAppender m,LogAppenderFinish m)
asyncAppender nm chld qsz = do
    tc <- liftIO $ newTBChanIO qsz -- chan of (IO ())
    tid <- liftIO $ forkIO $ childLogger chld tc
    return (nm, \m lvl msg args -> liftIO $ atomically $ writeTBChan tc (m,lvl,msg,args),liftIO $ killThread tid)
