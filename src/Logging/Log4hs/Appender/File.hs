module Logging.Log4hs.Appender.File (fileAppender) where

import           Control.Monad          (unless, when)
import           Control.Monad.IO.Class
import           Data.Text              (unpack)
import           Logging.Log4hs
import           System.IO



fileAppender :: MonadIO m => Bool -> Bool -> FilePath -> Bool -> Layout m -> m (LogAppender m,LogAppenderFinish m)
fileAppender append buffer file flush layout = do
    h <- liftIO $ do
        h' <- openFile file (if append then AppendMode else WriteMode)
        unless buffer $ hSetBuffering h' NoBuffering
        return h'
    return (\m lvl msg args -> do
                txt <- layout m  lvl msg args
                liftIO $ hPutStr h (unpack txt)
                when flush (liftIO $ hFlush h)
           ,liftIO $ hClose h
           )
